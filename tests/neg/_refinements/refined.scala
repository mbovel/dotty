import scala.language.experimental.refinements
import annotation.refined

type Pos = Int @refined[String](_ == "") // error: Malformed refinement.