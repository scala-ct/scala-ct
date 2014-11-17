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
  case class Self(v: Tree)

  val (inline, inlinestatic, static, dynamic) =
    (typeOf[ch.epfl.scalainline.inline],
      typeOf[ch.epfl.scalainline.inlinestatic],
      typeOf[ch.epfl.scalainline.static],
      typeOf[ch.epfl.scalainline.dynamic])
  val variants = Set(inline, inlinestatic, static, dynamic)

  object Variant {
    def unapply(x: Any): Option[(Type, Type)] = x match {
      case t: Tree if t.attachments.contains[TypeVariant] => unapply(t.attachments.get[TypeVariant].get.tpe)
      case AnnotatedType(List(Annotation(tpe, _, _), _*), t) if variants.exists(_ =:= tpe) => Some(t, tpe)
      case t: Type => Some(t, dynamic)
      case t: Tree => Some(t.tpe, dynamic)
    }
  }

  def variant(tree: Any): Type = tree match {
    case Variant(_, y) => y
  }

  /*
   * Convenience method for traversing annotated types.
   */
  def mapType(tpe: Type, f: (Type, Type) => Type): Type = tpe.widen match {
    case TypeRef(prefix, tp, args) if tp.isTypeParameter => // TODO find a better way
      TypeRef(prefix, tp, args)

    case TypeRef(prefix, tp, args) =>
      AnnotatedType(List(AnnotationInfo(f(dynamic, tpe), Nil, Nil)), TypeRef(prefix, tp, args.map(mapType(_, f))))

    case AnnotatedType(List(Annotation(annTpe, _, _), _*), TypeRef(prefix, tp, args)) if variants.exists(_ =:= annTpe) =>
      AnnotatedType(List(AnnotationInfo(f(annTpe, tpe), Nil, Nil)), TypeRef(prefix, tp, args.map(mapType(_, f))))

    case MethodType(l, resTp) => // TODO not sure about this
      AnnotatedType(List(AnnotationInfo(f(dynamic, tpe), Nil, Nil)), MethodType(l, mapType(resTp, f)))

    case NullaryMethodType(_) => // TODO do not know how to handle this
      tpe.widen

    case PolyType(vars, tpe) =>
      AnnotatedType(List(AnnotationInfo(f(dynamic, tpe), Nil, Nil)), PolyType(vars, mapType(tpe, f)))

    case _ => throw new RuntimeException("Unexpected Type " + showRaw(tpe))
  }

  def promoteType(tpe: Type, to: Type): Type = mapType(tpe, (_, tpe) => to)

  def promoteOne(tpe: Type, to: Type): Type =
    AnnotatedType(List(AnnotationInfo(to, Nil, Nil)), tpe)

  object MultipleApply {
    def unapply(value: Tree): Option[(Tree, List[Tree])] = value match {
      case Apply(x, y) =>
        Some(x match {
          case MultipleApply(rx, ry) =>
            (rx, ry ::: y)
          case _ =>
            (x, y)
        })
      case _ => None
    }
  }

  val name = "partial-evaluation"
  val description = "Partially evaluates Scala trees according to the type annotations."
  val components = List[PluginComponent](Component)

  sealed trait DebugContext
  case object Default extends DebugContext
  case object AppTpe extends DebugContext
  case object Interpreter extends DebugContext
  case object ValDefs extends DebugContext
  case object IfStatement extends DebugContext
  case object Idents extends DebugContext
  case object SelectContext extends DebugContext
  case object News extends DebugContext
  case object Blocks extends DebugContext

  var debugging = false
  val debugContexts: Set[DebugContext] = Set(Default, AppTpe)
  def debug(msg: String, context: DebugContext = Default): Unit =
    if (debugContexts.contains(context) && debugging) println(msg)

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
        case Variant(_, `inline`) => true
        case _                    => false
      }

      def variantType(tree: Tree): Type = tree match {
        case t: Tree if t.attachments.contains[TypeVariant] =>
          t.attachments.get[TypeVariant].get.tpe
        case Ident(_) if promotedTypes.contains(tree.symbol) => variantType(promotedTypes(tree.symbol)._1)
        case Ident(_)                                        => promoteType(tree.tpe, dynamic)
        case _ =>
          debug(s"<warn> Have no variant for: $tree: ${tree.tpe}")
          tree.tpe
      }

      def value[T](t: Tree): T = {
        if (variant(t) =:= dynamic) throw new RuntimeException(s"Trying to fetch a value of the dynamic value: ${t}.")
        (t match {
          case t if t.attachments.contains[TreeValue] => t.attachments.get[TreeValue].get
          case Literal(Constant(x))                   => x
          case Ident(_)                               => value[T](promotedTypes(t.symbol)._1)
        }).asInstanceOf[T]
      }

      def inlinePackageObject(t: Tree) = t.symbol.owner.isType &&
        t.symbol.owner.asType == typeOf[ch.epfl.scalainline.`package`.type].typeSymbol

      def inlineTransformed[C <: Context](c: C)(body: c.Tree)(
        tr: (c.Tree, c.internal.TypingTransformApi) => c.Tree)(
          tparamsMap: Map[c.Symbol, c.Type])(paramss: List[List[c.Tree]], args: List[c.Tree]): c.Tree = {
        import c.universe._
        import c.universe.internal._, decorators._
        val params = paramss.flatten
        val paramsMap = (params zip args).map {
          case (param @ q"${ _ } val $name: ${ _ } = ${ _ }", arg) =>
            val temp = c.freshName(name)
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

          case t: TypeTree =>
            // promote
            if (tparamsMap.contains(t.tpe.typeSymbol)) TypeTree(tparamsMap(t.tpe.typeSymbol))
            else t
          case _ =>
            api.default(tree)
        })

        q"""{
          ..${paramsMap.values.map(_._2)}
          ${c.internal.typingTransform(inlinedBody)(tr)}
        }"""
      }

      def inlineMethod(c: Context)(f: c.Tree, self: c.Tree)(targs: List[c.Type])(args: List[c.Tree]): c.Tree = {
        import c.universe._
        import c.universe.internal._, decorators._
        val q"${ _ } def ${ _ }[..$tparams](...$paramss): $tpe = $body" = f
        val tpMap = (tparams zip targs).map(x => (x._1.symbol, typeOf[Int])).toMap // TODO
        inlineTransformed[c.type](c)(body)((tree, api) => tree match {
          case This(_) => self
          case _       => api.default(tree)
        })(tpMap)(paramss, args)
      }

      def inlineLambda(c: Context)(f: c.Tree, args: List[c.Tree]): c.Tree = {
        import c.universe._
        import c.universe.internal._, decorators._
        val q"(..$params) => $body" = f
        inlineTransformed[c.type](c)(body)((tree, api) => api.default(tree))(Map())(List(params), args)
      }

      // creates a constant out of the value
      def const(t: Any): Tree =
        transform(localTyper.typed(Literal(Constant(t))))

      def inlineTree(valueOrTree: Any): Tree = valueOrTree match {
        case tree: Tree => tree
        case value      => const(value)
      }

      /*
       * All trees in the current run.
       */
      val allTrees: Seq[Tree] = global.currentRun.units.map(_.body).toSeq
      def eval(tree: Tree): Tree = {
        val (engine, (value, env)) = interpret.withDefs(context)(allTrees)(tree)
        val finalRes = if (tree.tpe <:< typeOf[scala.AnyVal]) {
          val (evalRes, _) = value.asInstanceOf[engine.JvmValue].reify(env.asInstanceOf[engine.Env])
          inlineTree(evalRes).updateAttachment(TypeVariant(promoteType(tree.tpe, inline)))
        } else tree.updateAttachment(TreeValue(value, Some(env), false))

        assert(variant(finalRes) == inline, s"Everything interpreted must be inline: culprit $tree.")
        finalRes
      }

      def functionAnnotation(methodSym: Symbol): Type = {
        val allVariants = methodSym.annotations.filter(_.tree.tpe <:< typeOf[ch.epfl.scalainline.Variant])
        if (allVariants.size > 1) error("Function should have only one inline argument.")
        allVariants.headOption.map(_.tree.tpe).getOrElse(dynamic)
      }

      /*
       * Fetching sources.
       */
      def symSourceWithModuleClasses = global.currentRun.symSource.map(x => (if (x._1.isModule) x._1.moduleClass else x._1, x._2))

      def canInline(sym: Symbol): Boolean =
        sym.ownerChain.find(symSourceWithModuleClasses.contains(_)).nonEmpty ||
          sym.ownerChain.find(global.currentRun.symSource.contains(_)).nonEmpty ||
          sym.owner == typeOf[Function1[_, _]].typeSymbol || sym.owner == typeOf[Function2[_, _, _]].typeSymbol

      def fetchBody(sym: Symbol): Option[Tree] = {
        val classSym = sym.ownerChain.find(symSourceWithModuleClasses.contains(_)).orElse(
          sym.ownerChain.find(global.currentRun.symSource.contains(_)))
        classSym.flatMap { classSym =>
          val file = (if (global.currentRun.symSource.contains(classSym))
            global.currentRun.symSource
          else symSourceWithModuleClasses)(classSym)

          val unit = global.currentRun.units.find(_.source.file == file).get
          val method = unit.body.find {
            case df: DefDef => df.symbol == sym
            case _          => false
          }

          method
        }
      }

      def minimize(block: Tree): Tree = {
        minimize(context)(block.asInstanceOf[context.Tree]).asInstanceOf[Tree]
      }

      def minimize(c: Context)(block: c.Tree): c.Tree = {
        import c.universe._
        import c.universe.internal._, decorators._

        val vals: mutable.Map[Symbol, c.Tree] = mutable.Map()
        val minimizedBody = c.internal.typingTransform(block) { (tree, api) =>
          tree match {
            case q"${ _ } val $valName: ${ _ } = $body" if (!tree.symbol.isParameter) =>
              val newBody = api.default(body)
              vals += (tree.symbol -> newBody)
              q"()"
            case Ident(x) if (vals.contains(tree.symbol)) =>
              vals(tree.symbol)
            case _ =>
              api.default(tree)
          }
        }

        minimizedBody match {
          case Block(_, res) => res
          case tree          => tree
        }
      }

      def application(sym: Symbol, tree: Tree, lhs: Tree, args: List[Tree]): Tree = {
        // Typechecking
        case class Constraint(tp: Type, level: Int)
        val tparams = sym.asMethod.typeParams.map(x => (x, mutable.Set[Constraint]())).toMap

        def compose(m1: Map[Symbol, Set[Constraint]], m2: Map[Symbol, Set[Constraint]]): Map[Symbol, Set[Constraint]] =
          (m1.keySet ++ m2.keySet).map(sym => (sym -> (m1.getOrElse(sym, Set()) ++ m2.getOrElse(sym, Set())))).toMap

        def params(expectedTp: Type, tp: Type): Map[Symbol, Set[Constraint]] = {
          (expectedTp, tp) match {
            case (Variant(TypeRef(_, ptp, pargs), variantE), Variant(TypeRef(_, _, args), variantA)) =>
              val constraints = (if (tparams.contains(ptp))
                Seq((ptp -> Set(Constraint(variant(tp), if (variantE != dynamic) 1 else 0))))
              else Seq()).toMap
              (pargs zip args).foldLeft(constraints)((agg, tps) => compose(agg, (params _).tupled(tps)))
          }
        }
        debug(s"------------------------------ ${sym.owner}.${sym.name} ---------------------------------")
        val constraints = (sym.asMethod.paramLists.flatten.map(_.tpe) zip args.map(variantType))
          .foldLeft(Map[Symbol, Set[Constraint]]())((agg, x) => compose(agg, (params _).tupled(x)))

        val minimizedConstraints: Map[Symbol, Type] = constraints mapValues { constraints =>
          val (hiPri, loPri) = constraints partition (_.level == 1)
          val relevantConstraints = if (hiPri.isEmpty) loPri else hiPri
          // TODO set rules in stone with Denys. Not sure what to do with inlineable.
          relevantConstraints.foldLeft(inline) { (agg, cons) => lub(agg :: cons.tp :: Nil) }
        }

        def typecheck(arg: Tree, expectedTp: Type, tp: Type): Unit = (expectedTp, tp) match {
          case (Variant(TypeRef(_, ptp, pargs), variantE), Variant(TypeRef(_, _, args), variantA)) =>
            val expectedVariant =
              if (minimizedConstraints.contains(ptp)) minimizedConstraints(ptp)
              else variantE
            // TODO error
            if (expectedVariant <:< static && variantA =:= dynamic) warning(s"Argument $arg did not match inlinity expected: $expectedTp got: $tp.")
            (pargs zip args).foreach(tps => typecheck(arg, tps._1, tps._2))
        }

        // TODO Add promotion of static/inline to dynamic
        def coaerce(expectedTp: Type, tp: Type): Type = (expectedTp, tp) match {
          case (Variant(TypeRef(_, ptp, pargs), variantE), Variant(TypeRef(prefix, tpe, args), variantA)) =>
            val expectedVariant =
              if (minimizedConstraints.contains(ptp)) minimizedConstraints(ptp)
              else variantE

            val newArgs = (pargs zip args).map(tps => (coaerce _).tupled(tps))
            if (!(variantA =:= dynamic) && expectedVariant <:< variantA)
              promoteOne(TypeRef(prefix, tpe, newArgs), expectedVariant)
            else if (expectedVariant =:= inlinestatic && variantA =:= static)
              promoteOne(TypeRef(prefix, tpe, newArgs), inline)
            else promoteOne(TypeRef(prefix, tpe, newArgs), variantA)
        }

        val expectedTypes = sym.asMethod.paramLists.flatten.map { param =>
          if (isInline(lhs)) promoteType(param.tpe, inline)
          else param.tpe
        }

        val promoteArgs = (expectedTypes zip args).map {
          case (param, arg) =>
            typecheck(arg, param, variantType(arg))
            // if all is OK coaerce arguments
            val resultType = coaerce(param, variantType(arg))
            arg.updateAttachment(TypeVariant(resultType))
        }

        val methodSym = lhs.attachments.get[Self].map(_.v).flatMap { x =>
          x.tpe.typeSymbol.typeSignature.member(sym.asMethod.name).alternatives.find(alt => alt.typeSignature matches sym.asMethod.infoIn(x.tpe))
        }.getOrElse(sym)

        debug(s"Method body fetching: " + lhs.attachments.get[Self] + " " + methodSym.owner + " " + functionAnnotation(methodSym))
        val shouldInline = !sym.isConstructor &&
          (functionAnnotation(methodSym) =:= inline || // explicitly annotated
            // TODO Discuss with Denys what to do here... TODO Refine for nested types.
            (functionAnnotation(methodSym) =:= inlinestatic && (expectedTypes zip args).forall(x => !(variant(x._1) =:= inlinestatic) || variant(x._2) =:= inline)) || // function is inlinestatic and all inlinestatic args are satisfied
            isInline(lhs)) // lhs is promoted to inline (type checking checks the arguments)

        def withInline[T](cond: Boolean)(block: => T): T = {
          if (cond) inlineLevel += 1
          val res = block
          if (cond) inlineLevel -= 1
          res
        }

        withInline(isInline(lhs) && !sym.isConstructor) {
          val res = if (shouldInline) {
            debug("Args before:" + args.map(arg => s"$arg: ${arg.tpe}"), AppTpe)
            debug("Args after:" + promoteArgs.map(arg => s"$arg: ${arg.attachments.get[TypeVariant].get.tpe}"), AppTpe)
            List(typeOf[Int], typeOf[Int])
            val res = if (canInline(sym)) { // method
              val self = lhs.attachments.get[Self].map(_.v).getOrElse(EmptyTree)
              // here we have a method sym
              val inlined = if (methodSym.owner == typeOf[Function1[_, _]].typeSymbol || methodSym.owner == typeOf[Function2[_, _, _]].typeSymbol) {
                inlineLambda(context)(self, promoteArgs)
              } else {
                inlineMethod(context)(
                  fetchBody(methodSym).get.asInstanceOf[context.Tree], self.asInstanceOf[context.Tree])(
                    List(typeOf[Int], typeOf[Int]))(
                      promoteArgs.asInstanceOf[List[context.Tree]])
              }
              debug(s"Inlining ${sym.owner}.$sym: ${show(inlined)}", AppTpe)
              val res = transform(localTyper.typed(inlined))
              debug(s"Inlined ${sym.owner}.$sym: ${show(res)}: ${variantType(res)}", AppTpe)
              res
            } else { // interpretation of the unavailable functions
              val interpretee = treeCopy.Apply(tree, lhs, promoteArgs.map { arg =>
                val argTree = if (variant(arg) =:= inline) inlineTree(arg)
                else {
                  val res = localTyper.typed(q"()")
                  res.updateAttachment(TreeValue(arg, None, false))
                  res
                }
                // if the argument is a function with types that are dynamic
                if (global.definitions.isFunctionType(argTree.tpe) && argTree.tpe.typeArgs.forall(variant(_) == dynamic)) {
                  // make a callback from the interpreter
                  val callback: List[Tree] => Tree = args => {
                    transform(localTyper.typed(inlineLambda(context)(arg, args)))
                  }
                  argTree.updateAttachment(TreeValue(callback, None, false))
                }
                argTree
              })
              debug(s"Interpret: $interpretee", Interpreter)
              eval(interpretee)
            }

            def promote(returnType: Type, tpe: Type): Type = (returnType, tpe) match {
              case (TypeRef(_, etp, eargs), Variant(TypeRef(prefix, tp, args), variant)) =>
                // TODO resolve this issue when minimizedConstraints does not contain it
                val resultInlinity = if (etp.isTypeParameter && minimizedConstraints.contains(etp))
                  minimizedConstraints(etp)
                else variant
                val promotedType = tp
                // val promotedType = if (etp.isTypeParameter && tpMap.contains(tp.typeSymbol)) tpMap(tp)
                // else tp
                AnnotatedType(List(AnnotationInfo(resultInlinity, Nil, Nil)),
                  TypeRef(prefix, promotedType, (eargs zip args).map((promote _).tupled)))
            }

            // typing the return type
            val returnType = sym.asMethod.returnType
            debug(s"Return type: ${show(returnType)}", AppTpe)
            // val finalRes = if (variant(res) =:= inline) { // minimize the result
            val finalRes = localTyper.typed(minimize(res).asInstanceOf[Tree])
            // } else res
            val finalVariant = TypeVariant(promote(returnType, variantType(res)))
            debug(s"Promoted return type for $finalRes: $finalVariant", AppTpe)
            finalRes.updateAttachment(finalVariant)
          } else if (sym.isConstructor && isInline(lhs)) {
            val res = treeCopy.Apply(tree, lhs, promoteArgs)
            val returnType = sym.asMethod.returnType
            res.updateAttachment(TypeVariant(promoteType(returnType, inline)))
          } else if (!sym.isConstructor) {
            val res = treeCopy.Apply(tree, lhs, promoteArgs)
            res.updateAttachment(TypeVariant(promoteType(tree.tpe, dynamic)))
          } else {
            super.transform(tree)
          }

          res
        }
      }

      var inlineLevel: Int = 0
      def byMode(tp: Type) = if (inlineLevel == 0) tp else inline

      override def transform(tree: Tree): Tree = tree match {
        // TODO Gross Hack (we need access to underlying objects here or in the interpreter)
        case q"Nil == Nil" => transform(localTyper.typed(q"_root_.ch.epfl.scalainline.inline(true)"))
        case q"$x == Nil"  => transform(localTyper.typed(q"_root_.ch.epfl.scalainline.inline(false)"))

        // constants and lambdas are static
        case Literal(Constant(x)) =>
          if (!isInline(tree)) // do not delete inlinity
            // TODO remove the typecheck
            tree.updateAttachment(TypeVariant(promoteType(localTyper.typed(tree).tpe.widen, byMode(static))))

          tree.updateAttachment(Self(tree))

        case Function(vparams, body) =>
          val res = treeCopy.Function(tree, vparams.map(x => transform(x).asInstanceOf[ValDef]), transform(body))
          res.updateAttachment(TypeVariant(promoteOne(tree.tpe, byMode(static))))
          res.updateAttachment(Self(res))
          res

        case New(sel) =>
          val newSel = transform(sel)
          debug(s"New(sel: ${variant(newSel)}): ${promoteOne(tree.tpe, byMode(static))}", News)
          treeCopy.New(tree, newSel).updateAttachment(TypeVariant(promoteOne(tree.tpe, byMode(static))))

        case Block(body, res) =>
          debug("Block: " + show(res), Blocks)
          val (newBody, newRes) = (body.map(x => transform(x)), transform(res))
          treeCopy.Block(tree, newBody, newRes)
            .updateAttachment(TypeVariant(variantType(newRes)))

        case q"new ${ _ }[..${ tparams }](..${ params })" if tree.attachments.get[Self].isEmpty =>
          val finalRes = transform(tree.updateAttachment(Self(tree)))
          finalRes.updateAttachment(Self(finalRes))

        /*
         * Inlines access to direct constructor fields.
         * NOTE: This could also be done by the interpreter
         */
        case Select(obj @ q"new ${ _ }[..${ tparams }](..${ params })", field) if obj.symbol.asMethod.paramss.head.exists(x => x.name.toString == field.toString.trim) =>
          (obj.symbol.asMethod.paramss.head zip params).find(_._1.name.toString == field.toString.trim).map(_._2).get

        case Select(x, y) =>
          val nx = transform(x)
          def copy = treeCopy.Select(tree, nx, y)
          val res = nx match {
            case nx if nx.symbol != null && nx.symbol.hasPackageFlag =>
              copy.updateAttachment(TypeVariant(promoteType(tree.tpe, byMode(static))))

            case Variant(_, `inline`) if tree.symbol != null && tree.symbol.isMethod && tree.symbol.asMethod.paramss.isEmpty => // interpret
              val nonPolymorphicSymbol = localTyper.typed(Select(nx, y)).symbol
              application(nonPolymorphicSymbol, localTyper.typed(copy), nx, Nil)

            case Variant(_, variant) =>
              copy.updateAttachment(TypeVariant(promoteType(tree.tpe, variant)))
          }
          debug(s"Select(x:${variantType(nx)}, $y): ${variant(res)}", SelectContext)

          if (tree.symbol != null && tree.symbol.isModule) res.updateAttachment(Self(tree))
          else nx.attachments.get[Self].foreach(self => res.updateAttachment(self))

          res

        case TypeApply(x, targs) =>
          val lhs = transform(x)
          val res = treeCopy.TypeApply(tree, lhs, targs.map(transform(_)))
          lhs match {
            case Variant(_, variant) =>
              res.updateAttachment(TypeVariant(promoteType(tree.tpe, variant)))
          }
          lhs.attachments.get[Self].foreach(self => res.updateAttachment(self))
          res

        case Ident(x) if tree.symbol.isModule =>
          tree.updateAttachment(TypeVariant(promoteType(tree.tpe, byMode(static))))
          tree.updateAttachment(Self(tree))

        case Ident(x) if promotedTypes.contains(tree.symbol) =>
          val res = (if (isInline(promotedTypes(tree.symbol)._1)) {
            promotedTypes(tree.symbol)._1
          } else super.transform(tree))
          debug(s"$x = $res: ${promotedTypes(tree.symbol)._2}", Idents)
          res.updateAttachment(TypeVariant(promotedTypes(tree.symbol)._2))
          res.updateAttachment(Self(promotedTypes(tree.symbol)._1))
          res

        case DefDef(_, _, _, vparams, _, _) =>
          val paramssTypes = vparams.map(p => p.map { case ValDef(_, _, tpe, _) => tpe })
          // for now treating only non-curried functions
          val skipFunction = paramssTypes.exists(_.exists(_.tpe.exists {
            case Variant(_, v @ (`inline` | `inlinestatic`)) => true
            case _ => false
          }))
          if (skipFunction) tree else super.transform(tree)

        /*
         * Prints trees of the argument - used for debugging partial evaluation.
         */
        case Apply(x, args) if inlinePackageObject(x) && x.symbol.name.toString == "showCode" =>
          val res = transform(args.head)
          localTyper.typed(q"new String(${showCode(res)})")

        /*
         * CT intrinsic promotes the types of a shared object such that:
         *   - all parameters are promoted to inline
         */
        case Apply(x, args) if inlinePackageObject(x) && x.symbol.name.toString == "ct" =>
          val trArg = transform(args.head)
          if (!(variant(trArg) <:< static)) error("inline can only contain static values.")
          val res = eval(trArg).updateAttachment(TypeVariant(promoteType(tree.tpe, inline)))
          assert(variant(res) =:= inline)
          res

        case Apply(x, args) if inlinePackageObject(x) && x.symbol.name.toString == "debug" =>
          debugging = true
          val res = transform(args.head)
          debugging = false
          res

        /*
         * Inline intrinsic promotes the types of a shared object such that:
         *   - all functions become inline
         *   - all non-generic arguments of functions become inline
         */
        case Apply(x, args) if inlinePackageObject(x) && x.symbol.name.toString == "inline" =>
          val trArg = transform(args.head)

          if (!(variant(trArg) <:< static)) error("inline can only contain static values.")
          trArg.updateAttachment(TypeVariant(promoteType(tree.tpe, inline)))

        case Apply(x, args) if x.symbol != null =>
          val (lhs, transArgs) = (transform(x), args.map(transform(_)))
          // TODO Need type arguments of the application
          // TODO Need type arguments of self
          // TODO handle multiple application
          application(x.symbol, tree, lhs, transArgs)

        /*
        * For valdefs that (in expressions position) we update the type
        * according to the rhs' inlinity. The rhs is stored to `promotedTypes`
        * for fetching by following Idents.
        */
        case ValDef(a, b, c, d) =>
          val rhs = transform(d)
          val newTpe = rhs.attachments.get[TypeVariant].map(_.tpe)
          newTpe.foreach(tpe => promotedTypes += (tree.symbol -> ((rhs, tpe))))
          debug(s"valdef rhs = $rhs: $newTpe", ValDefs)

          val newTypeTree = newTpe.map(TypeTree(_)).getOrElse(c)
          localTyper.typed(copyValDef(tree)(a, b, newTypeTree, rhs))

        /*
         * Type checking: if not inline, the result type is a lub of all branches and the condition.
         * Transformation: First transform the condition, if inline remove the if, and then
         * transform the branches. This prevents infinite recursion.
         */
        case If(c, t, e) =>
          val nc = transform(c)
          debug(s"if c = $nc: ${variantType(nc)}", IfStatement)
          if (isInline(nc))
            if (value[Boolean](nc)) transform(t)
            else transform(e)
          else {
            val (thn, els) = (transform(t), transform(e))
            val result = treeCopy.If(tree, nc, thn, els)
            val condType = promoteType(lub(thn.tpe :: els.tpe :: Nil), variant(nc))
            val resType = lub(condType :: variant(thn) :: variant(els) :: Nil)
            result.updateAttachment(TypeVariant(resType))
            result
          }

        case _ =>
          super.transform(tree)
      }
    }
  }
}
