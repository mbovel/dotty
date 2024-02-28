extension (x: Any) def runtimeChecked: x.type = x


def main =
  val x: Int = ???
  val y: {v: Int with v == 10} = x.runtimeChecked
  //  val y: Int @qualified[Int](v => v == 10) = x.runtimeChecked[{v: Int with v == 10}]
