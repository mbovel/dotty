type Pos6 = {x: (Int with x > 0) with x < 10} // error
type Pos7 = {x: Int with x > 0 with x < 10} // error
