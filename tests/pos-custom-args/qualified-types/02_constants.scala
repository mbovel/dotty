import language.experimental.setNotation

def Main =
  val v1: {v: Int with v < 2} = 0
  val v2: {v: Int with v >= 0} = 0
  val v3: {v: Int with v <= 0} = 0
  val v4: {v: Int with v < 1} = 0
  val v5: {v: Int with v > -1} = 0
  val v6: {v: Int with v != 1} = 0
