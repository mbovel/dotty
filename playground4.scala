type Pos = {v: Int with v >= 0}

abstract class Vec:
  val size: Pos

def concat(v1: Vec, v2: Vec): Vec {val size: {v: Int with v == v1.size + v2.size}} = ???

def main =
  val v1: Vec = ???
  val v2: Vec {val size: 3} = ???
  val v3: Vec {val size: {v: Int with v == v1.size + 3}} = concat(v1, v2)
  ()

