
class MyRange(val from: Int, val to: Int with to > from):
  def foreach(f: {i: Int with i >= from && i < to} => Unit): Unit =
    def loop(i: Int with i >= from && i <= to): Unit =
      i match
        case i: {i: Int with i >= from && i < to} => f(i); loop(i + 1)
        case _ => ()
    loop(from)

def myRange(a: Int, b: Int with b > a): MyRange {val from: a.type; val to: b.type} =
  new MyRange(a, b).asInstanceOf[MyRange {val from: a.type; val to: b.type}]

def main =
  val r = MyRange(0, 10)
  r.foreach: j =>
    val y: Int with y >= r.from && y <= r.to = j
    ()
