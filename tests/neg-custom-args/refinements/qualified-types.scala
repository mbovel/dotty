
object setNotation:
  import language.experimental.setNotation

  type Pos7 = {x: Int with x > 0 with x < 10} // error

  type Repeated = Double with true with true // error

  //TODO: Fix this ? Could be syntactic sugar for {x: {x2: Int with x2 > 0} with x < 10}
  type Pos6 = {x: (Int with x > 0) with x < 10} // error: Not found: x

object postfixLambda:
  import language.experimental.postfixLambda

  def bar(x: Int with y => x > 0) = 0 // error

  type Repeated = Double with true with true // error

  // TODO: fix the following ?
  def f: Int => Boolean = x => true
  type Call = Int with f // error

  val x = ???
  type ParensAmbiguity1 = Int with (x: Int) => true // error: qualified types may not be followed by '=>'
  type ParensAmbiguity2 = Int with (x) => true      // error: qualified types may not be followed by '=>'  // error: Required: Boolean
  type ParensAmbiguity3 = Int with (x) ?=> true     // error: qualified types may not be followed by '?=>' // error: Required: Boolean
