def log(x: Int with x > 0): Int = ???

def logSafe(y: Int) =
  y match
    case x: (Int with x > 0) => Some(log(x))
    case _      => None
