type Pos = {x: Int with x >= 0}

def takePos(x: Pos): Pos = x

def main =
  val y: {x: Int with 0 <= x} = takePos(10)
