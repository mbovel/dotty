def f[T] = ()

@main def Test: Unit =
  val n = 10

  if n.isInstanceOf[({v: Int with v > 0})] then println("n is an Int with value > 0")
  else println("n is not an Int with > 0")

  /*

  // without () it doesn't compile
  if n.isInstanceOf[ ({v: Int with v > 0}) | String] then println("n is an Int with value > 0 or a String")
  else println("n is not an Int with > 0 or a String")


  */

  /*
    Fix to tree
    Fix with string
    Rename phases (runtimeChecked, eventuallyQualifiedType) put inside qualifier
    Do pr on refinments not on dotty and don't deal with conflicts
  */