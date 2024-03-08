class MyRange(val from: Int, val to: Int with to > from):
  def foreach(f: {i: Int with i >= from && i < to} => Unit): Unit =
    var i: Int with i >= from && i < to = from
    while i < to do
      f(i)
      i = (i + 1).asInstanceOf[{i2: Int with i2 >= from && i2 < to}] // Need flow-sensitive typing here

def main =
  MyRange(0, 10).foreach: j =>
    val y: Int with y >= 0 && y < 10 = j
