def main =
  val x = 10
  x match
    case y: {z: Int with z == 10} =>
      y
