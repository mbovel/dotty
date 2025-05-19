
def getInt(): Int = 1

@main def Test =
  val x: {v: Int with v == 1} = getInt().runtimeChecked
