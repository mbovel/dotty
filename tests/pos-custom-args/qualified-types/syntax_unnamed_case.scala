def Test(x: Any) =
  x match
    case y: Int with y > 0 => println(s"$x is Int with x > 0")
    case _ => ()

  x match
    case (y: Int with y > 0, z: Int with z > 0) => println(s"$x is Int with x > 0")
    case _ => ()
