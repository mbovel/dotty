/*
val b: Boolean = false
type Pos = {x: Int with b}
val scrut: Any = 4
val y = scrut match
  case x: Pos => "It Pos"
  case _ => "It not Pos"

val z = if 4.isInstanceOf[Pos] then "It Pos" else "It not Pos"
*/

import scala.language.experimental.postfixLambda

type OverlyVerbose = Int with (x) => true // crash

//def bar(x: Int with y => x > 0) = 0 // error: Cyclic reference involving val x
