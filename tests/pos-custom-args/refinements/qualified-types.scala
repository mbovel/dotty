
object setNotation:
  import language.experimental.setNotation

  val b1 = true
  val b2 = true

  type T0 = Int with b1
  type T1 = Int with b1 && b2
  type T2 = Int with
    b1 && b2
  type T3 = Int with
      b1 &&
      b2


  (): (Unit with b1)

object postfixLambda:
  import language.experimental.postfixLambda
  val b1 = true
  val b2 = true

  type T0 = Int with b1
  type T1 = Int with b1 && b2
  type T2 = Int with x => b1 && b2
  type T3 = Int with x =>
      b1 &&
      b2

  (): (Unit with b1)

  // Not recommended
  type Test = Boolean with b
    => false


  /** The following is allowed, to keep consistency with
   *  https://github.com/lampepfl/dotty/issues/18014
   */
  type T4 = Int with
    x =>
      true
