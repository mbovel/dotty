import language.experimental.setNotation

import annotation.qualified

def id(x: Int): Int @qualified[Int](v => v == x) = x
def equal(x: Int, y: {v: Int with v == x}): Int = y

def Main =
  val x: Int = ???

  val v1 =
    x match
      case y: {v: Int with v == 10} => y

  val v2: {v: Int with v == 10} =
    x match
      case y: {v: Int with v == 10} => y
      case _ => throw new Error()


