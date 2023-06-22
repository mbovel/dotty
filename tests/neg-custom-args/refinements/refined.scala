import annotation.refined

type Pos = Int @refined[String](_ == "") // error: Malformed refinement.

// TODO: Fix me
type DoubleKing = String  @refined[String](_ != "") @refined[String @refined[String](_ != "")](_.head == 'K') // error: Malformed refinement
