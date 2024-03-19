type Pos = {x: Int with x >= 0}

def takePos(x: Pos) = x

def main =
  takePos(10)
