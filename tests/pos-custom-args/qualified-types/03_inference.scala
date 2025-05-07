def f[T](x: T, y: T) = x

def test =
  val v1: {v: Int with v < 2} = ???
  val v2: {v: Int with v < 3} = v1
  f(v1, v2)


  //val v3: {v: Int with v < 2} = v2
  //val v4: {v: Int with v < 3} = v1
