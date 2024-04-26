class A(val s: String) extends AnyVal {
  //def f = eq("hello, world")  // missing argument list for method eq in object Predef

  def g = synchronized { println("hello, world") }
}
