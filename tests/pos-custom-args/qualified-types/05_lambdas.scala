def apply(x: Int)(f: {y: Int with y == x} => Unit) = f(x)

def main =
  apply(10): (x: {y: Int with y == 10}) =>
    val z: Int with z == 10 = x
    ()

  apply(10): x =>
    val z: Int with z == 10 = x
    ()
