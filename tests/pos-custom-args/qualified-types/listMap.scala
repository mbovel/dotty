type Pos = {res: Int with res >= 0}

def abs(x: Int): Pos = Math.abs(x).asInstanceOf[Pos]

def test: List[{res: Int with res >= 0}] = List(-1, 2, 3).map(abs)
