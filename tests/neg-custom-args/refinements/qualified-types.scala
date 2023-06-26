
object setNotation:
  import language.experimental.setNotation

  type Pos6 = {x: (Int with x > 0) with x < 10} // error: Not found: x
  type Pos7 = {x: Int with x > 0 with x < 10} // error

  type Repeated = Double with true with true // error

  def foo(x: {y: Int with x > 0}) = ???// error: Cyclic reference involving val x

object postfixLambda:
  import language.experimental.postfixLambda

  def bar(x: Int with y => x > 0) = 0 // error: Cyclic reference involving val x

  def f(x: Int with x > _) = ??? // error: Cyclic reference involving val x

  type Repeated = Double with true with true // error

  val x = ???
  type ParensAmbiguity1 = Int with (x: Int) => true // error: Qualified types may not be directly followed by '=>'
  type ParensAmbiguity2 = Int with (x) => true      // error: Qualified types may not be directly followed by '=>'  // error: Required: Boolean
  type ParensAmbiguity3 = Int with (x) ?=> true     // error: Qualified types may not be directly followed by '?=>' // error: Required: Boolean

  type Nesting = Int with { val y: Int with _ > 0 = ??? ; x => x > y } // error

  type MultipleWildcards = Int with _ > _ // error: Qualified type's qualifier contains 2 wildcards ('_'), when the maximum is 1

  def foo(x: Int with y => x > 0) = ???// error: Cyclic reference involving val x

  /** The following is disallowed, to keep consistency with
   *  https://github.com/lampepfl/dotty/issues/18014
   *  See https://github.com/lampepfl/dotty/issues/18042 for a better error message
   */
  type T4 = Int with
    x => // error: could not infer the type of the parameter x
      true

  // TODO: fix the following ?
  def f: Int => Boolean = x => true
  type Call = Int with f // error
