def abs(x: Int) = Math.abs(x).asInstanceOf[{res: Int with res > 0}]

def test: List[{it: Int with it > 0}] = List(1, 2, 3).map(abs)
