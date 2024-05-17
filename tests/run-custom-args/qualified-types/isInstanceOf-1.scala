def f[T] = ()

def Test =
  val n: Any = ???

  //this fail, the errors trigger radomly os I don't know where there is a problem
  if n.isInstanceOf[{v: Int with v > 0}] then println("n is an Int with value > 0")
  else println("n is not an Int with > 0")

  /*

  if n.isInstanceOf[{v: Int with v == 10}] then println("n is an Int with value 10")
  else println("n is not an Int with value 10")

  if n.isInstanceOf[Int] then println("n is an Int")
  else println("n is not an Int")


  if n.isInstanceOf[{v: Int with v < 0}] then println("n is an Int with value < 0")
  else println("n is not an Int with < 0")


  if n.isInstanceOf[{v: String with v.isEmpty()}] then println("n is a empty String")
  else println("n is a not an empty String")


  if n.isInstanceOf[{v: String with v != ""}] then println("n is a not an empty String")
  else println("n is an empty String")

  */



  /*
    Fix to tree
    Fix with string
    Rename phases (runtimeChecked, eventuallyQualifiedType) put inside qualifier
    Do pr on refinments not on dotty and don't deal with conflicts
  */