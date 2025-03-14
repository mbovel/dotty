def opaqueSize[T](l: List[T]): Int = ???

def main =
  val x = 1
  val l: {v: List[Int] with opaqueSize(v) == 2} = ???
  val l2: {v: List[Int] with opaqueSize(v) == 2 * x} = ??? : {v: List[Int] with opaqueSize(v) == x + x}
