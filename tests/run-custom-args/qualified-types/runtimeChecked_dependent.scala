def foo(x: Int, y: {v: Int with v > x}): Unit = ()

def getInt(): Int = 1

@main def Test =
  // ok
  foo(1, 2.runtimeChecked)

  // ok
  val p: Int = 1
  foo(p, 0.runtimeChecked)

  // crash
  foo((1: Int), 2.runtimeChecked)

  // crash
  foo(getInt(), 2.runtimeChecked)
