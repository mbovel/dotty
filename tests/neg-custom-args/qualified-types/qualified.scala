import annotation.qualified

type Pos = Int @qualified[String](_ == "") // error: Malformed qualified type.

// TODO: Fix me
type DoubleKing = String  @qualified[String](_ != "") @qualified[String @qualified[String](_ != "")](_.head == 'K') // error: Malformed qualified type
