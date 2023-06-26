
object setNotation:
  import language.experimental.setNotation

object postfixLambda:
  import language.experimental.postfixLambda
  val b1 = true
  val b2 = true

  type T1 = Int with b1 && b2
  type T2 = Int with x => b1 && b2
  type T3 = Int with x =>
      b1 &&
      b2

  // Not recommended
  type Test = Boolean with b
    => false
