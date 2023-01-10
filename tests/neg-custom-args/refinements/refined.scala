import annotation.refined

type Pos = Int @refined[String](_ == "") // error: Malformed refinement.
