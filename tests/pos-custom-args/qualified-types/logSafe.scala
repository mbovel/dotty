def log(x: {it: Int with it > 0}): Int = ???

def logSafe(x: Int) =
  x match
    case x: {it2: Int with it2 > 1} => Some(log(x))
    case _      => None
