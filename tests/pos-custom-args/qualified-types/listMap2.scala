type Pos = {res: Int with res >= 0}

def abs(x: Int): Pos = Math.abs(x).asInstanceOf[Pos]

def test: List[{res: Int with res >= 0}] =
  val l = List(1, 2, 3)
  val l2 = l.map(abs)
  l2
