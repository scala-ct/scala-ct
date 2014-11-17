package ch.epfl.scalainline.plugin

import scala.reflect.macros.blackbox.Context
import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.transform.{ Transform, TypingTransformers }
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.annotation.StaticAnnotation
import scala.collection.mutable
import java.io._
import scala.reflect.interpreter._

class PartialEvaluationPlugin(val global: Global) extends Plugin {
  import global._

  case class TypeVariant(tpe: Type)
  val (inline, inlineable, static) =
    (typeOf[ch.epfl.scalainline.inline],
      typeOf[ch.epfl.scalainline.inlineable],
      typeOf[ch.epfl.scalainline.static])

  object Variant {
    def unapply(x: Any): Option[(Type, Type)] = x match {
      case t: Tree if t.attachments.contains[TypeVariant] => unapply(t.attachments.get[TypeVariant].get.tpe)
      case AnnotatedType(x, t) if x.exists(x => (x.atp <:< inline)) => Some(t, inline)
      case AnnotatedType(x, t) if x.exists(x => (x.atp <:< inlineable)) => Some(t, inlineable)
      case AnnotatedType(x, t) if x.exists(x => (x.atp <:< static)) => Some(t, static)
      case _ => None
    }
  }
  def variant(tree: Tree): Type = tree match {
    case Variant(_, y) =>
      y
  }
  def promoteType(tpe: Type, to: Type): Type = tpe.widen match {
    case TypeRef(prefix, tp, args) =>
      AnnotatedType(List(AnnotationInfo(to, Nil, Nil)), TypeRef(prefix, tp, args.map(promoteType(_, to))))
    case _ => tpe
  }

  def inlinity(x: Tree): Type = {
    x.attachments.get[TypeVariant] match {
      case Variant(_, variant) => variant
    }
  }

  val name = "partial-evaluation"
  val description = "Partially evaluates Scala trees according to the type annotations."
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with TypingTransformers with Transform {
    val global: PartialEvaluationPlugin.this.global.type = PartialEvaluationPlugin.this.global
    val runsAfter = List[String]("typer")
    val phaseName = PartialEvaluationPlugin.this.name
    def newTransformer(unit: CompilationUnit) = new PartialEvaluatorTransformer(unit)

    // here we make a macro context in order to use the Macro API
    val context = new {
      val universe: global.type = global
      val callsiteTyper: global.analyzer.Typer = global.typer
      val expandee = EmptyTree
    } with scala.reflect.macros.contexts.Context {
      val prefix = null
    }

    class PartialEvaluatorTransformer(unit: CompilationUnit)
      extends TypingTransformer(unit) {

      /**
       * Stores promoted types of trees that were encountered
       * during partial evaluation.
       */
      private val promotedTypes: mutable.Map[Symbol, (Tree, Type)] = mutable.HashMap.empty
      def isInline(t: Tree): Boolean = t match {
        case Select(x, y)         => isInline(x)
        case Ident(x)             => promotedTypes.contains(t.symbol)
        case Variant(_, `inline`) => true
        case x                    => false
      }

      def value[T](t: Tree): T = t match {
        case Literal(Constant(x)) => x.asInstanceOf[T]
        case Ident(_)             => value[T](promotedTypes(t.symbol)._1)
      }

      object InlineType {
        def unapply(x: Any): Option[Type] = x match {
          case AnnotatedType(x, t) if x.exists(x => (x.atp <:< inline)) => Some(t)
          case _ => None
        }
      }

      object InlineableType {
        def unapply(x: Any): Option[Type] = x match {
          case AnnotatedType(x, t) if x.exists(x => (x.atp <:< inlineable)) => Some(t)
          case _ => None
        }
      }

      def isInlineType(t: Type): Boolean = t match {
        case InlineType(_) => true
        case _             => false
      }

      def isInlineableType(t: Type): Boolean = t match {
        case InlineableType(_) => true
        case _                 => false
      }
      def coaerce(tree: Tree): Tree = {
        tree.updateAttachment(TypeVariant(promoteType(tree.tpe, inline)))
        assert(isInline(tree), s"The coaerced constant should always be inline: $tree!")
        tree
      }

      def inlinityMatches(t1: Type, t2: Type): Boolean =
        !(isInlineType(t1) ^ isInlineType(t2)) && ((t1, t2) match {
          case (TypeRef(_, _, args), TypeRef(_, _, args1)) => (args zip args1).forall((inlinityMatches _).tupled)
          case _ => true
        })

      def inlineMethod(c: Context)(f: c.Tree, args: List[c.Tree]): c.Tree = {
        import c.universe._
        import c.universe.internal._, decorators._
        val q"def ${ _ }(..$params): $tpe = $body" = f
        val paramsMap = (params zip args).map {
          case (param, arg) =>
            val temp = c.freshName(TermName(param.name.toString))
            val tempSym = localTyper.context.owner.asInstanceOf[Symbol].newTermSymbol(temp)
            val newArg = c.typecheck(arg) // typer does not set the type sometimes :/
            tempSym.setInfo(newArg.tpe.widen)

            val valDef = c.internal.valDef(tempSym, c.internal.changeOwner(arg, c.internal.enclosingOwner, tempSym))
            (param.symbol, (tempSym, valDef))
        }.toMap

        // put a name of the val
        val inlinedBody = c.internal.typingTransform(body)((tree, api) => tree match {
          case i @ Ident(_) if paramsMap contains tree.symbol =>
            val sym = paramsMap(tree.symbol)._1
            api.typecheck(q"$sym")
          case _ =>
            api.default(tree)
        })

        q"""{
          ..${paramsMap.values.map(_._2)}
          ${inlinedBody}
        }"""
      }

      // creates a constant out of the value
      def const(t: Any): Tree =
        transform(localTyper.typed(Literal(Constant(t))))

      override def transform(tree: Tree): Tree = tree match {
        // constants and lambdas are static
        case Literal(Constant(x)) =>
          if (!isInline(tree)) // do not delete inlinity
            tree.updateAttachment(TypeVariant(promoteType(tree.tpe, static)))
          tree
        case Function(vparams, body) =>
          val res = treeCopy.Function(tree, vparams.map(x => transform(x).asInstanceOf[ValDef]), transform(body))
          // TODO what about inner parameters? Should they be polymorphic? Should we introduce variables?
          res.updateAttachment(TypeVariant(promoteType(tree.tpe, static)))
          res
        case Select(x, y) =>
          val nx = transform(x)
          val res = treeCopy.Select(tree, nx, y)
          nx match {
            case Variant(_, x @ (`inline` | `static`)) =>
              res.updateAttachment(TypeVariant(promoteType(tree.tpe, x)))
            case _ =>
              res
          }

        case Ident(x) => // TODO this needs refinement
          // inline the value if the type is inline
          val res = if (isInline(tree))
            const(value[Any](tree)).updateAttachment(TypeVariant(promoteType(tree.tpe, inline)))
          else {
            treeCopy.Ident(tree, x)
          }
          res

        case DefDef(_, _, _, vparams, _, _) =>
          val paramssTypes = vparams.map(p => p.map { case ValDef(_, _, tpe, _) => tpe })
          // for now treating only non-curried functions
          val skipFunction = paramssTypes.head.exists(_.tpe.exists {
            case Variant(_, v @ (`inline` | `inlineable`)) => true
            case _                                         => false
          })
          if (skipFunction) tree else super.transform(tree)

        case Apply(x, args) if x.symbol.name.toString == "reify" =>
          val res = transform(args.head)
          localTyper.typed(q"new String(${showCode(res)})")

        case Apply(x, args) if x.symbol != null =>
          def isAnnotated(methodSym: Symbol): Boolean =
            methodSym.annotations.exists(_.tree.tpe <:< inline)

          def canInline(sym: Symbol): Boolean =
            sym.ownerChain.find(global.currentRun.symSource.contains(_)).nonEmpty

          def fetchBody(sym: Symbol): Option[Tree] = {
            val classSym = sym.ownerChain.find(global.currentRun.symSource.contains(_))
            classSym.flatMap { sym =>
              val file = global.currentRun.symSource(sym)
              val unit = global.currentRun.units.find(_.source.file == file).get
              val method = unit.body.find {
                case df: DefDef => df.symbol == x.symbol
                case _          => false
              }
              method
            }
          }

          val (lhs, transArgs) = (transform(x), args.map(transform(_)))
          val shouldInline = isAnnotated(x.symbol) || isInline(lhs)

          val res = if (shouldInline) {
            // typechecking (no generics and multiple parameter lists for now)
            val expectedTypes = x.symbol.asMethod.paramLists.head.map { param =>
              if (isInline(lhs)) promoteType(param.tpe, inline) else param.tpe
            }

            // TODO: no generics for now
            val promoteArgs = (transArgs zip expectedTypes).map {
              case (arg, param) =>
                // verify inline with non-static
                if (isInlineType(param) && variant(arg) != inline && variant(arg) != static)
                  error("Argument is not inline.")

                // promote static to inline and inlineable
                if (isInlineType(param) || isInlineableType(param)) coaerce(arg) else arg
            }

            if (canInline(x.symbol)) {
              val self = transform(x)
              // TODO transform self/this
              val res = inlineMethod(context)(
                fetchBody(x.symbol).get.asInstanceOf[context.Tree], promoteArgs.asInstanceOf[List[context.Tree]])
              transform(localTyper.typed(res))
            } else { // interpretation of the unavailable functions
              const((interpret(context)(treeCopy.Apply(tree, lhs, promoteArgs.map(x => const(value[Any](x)))))))
                .updateAttachment(TypeVariant(promoteType(tree.tpe, inline)))
            }
          } else super.transform(tree)
          // TODO attach the return type
          res
        case ValDef(a, b, c, d) =>
          val rhs = transform(d)
          val newTpe = if (isInline(rhs)) {
            val res = promoteType(c.tpe, inline)
            promotedTypes += tree.symbol -> (rhs, res)
            res
          } else c.tpe

          val res = copyValDef(tree)(a, b, TypeTree(newTpe), rhs)
          localTyper.typed(res)

        case If(c, t, e) =>
          val nc = transform(c)
          if (isInline(nc))
            if (value[Boolean](nc)) transform(t)
            else transform(e)
          else super.transform(tree)
        case _ =>
          super.transform(tree)
      }
    }
  }
}
