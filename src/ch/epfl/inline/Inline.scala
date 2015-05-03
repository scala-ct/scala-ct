package ch.epfl.scalact

import scala.annotation.StaticAnnotation

sealed trait Variant extends StaticAnnotation

/*
 * Internal marker trait that carries the information that a type is dynamic.
 * This is added since absence of this type requires special handling in several
 * locations.
 */
protected class dynamic extends Variant

/*
 * Internal marker trait that carries the information that a type is static, i.e., its
 * value can be known at compile time.
 * Static types are lambdas, constants, objects, methods on objects, results of static
 * methods that accept only static parameters.
 */
sealed class static extends dynamic

/*
 * Marker trait that denotes that all operations on the type will be executed at
 * compile-time.
 */
final class ct extends static

/*
 * Marker trait that denotes that if the function argument will be converted to
 * inline if Internal marker trait that carries the information that a type is dynamic.
 * This is added since absence of this type requires special handling in several
 * locations.
 */
final class ctstatic extends Variant
