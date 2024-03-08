type Int_0_127 = {x: Int with 0 <= x && x <= 127}

trait Matrix128[T]:
  def apply(i: Int_0_127, j: Int_0_127): T

def fromFunction[T](f : (Int_0_127, Int_0_127) => T): Matrix128[T] =
  new Matrix128:
    def apply(i: Int_0_127, j: Int_0_127) = f(i, j)

def diagonal: Matrix128[Int] =
  fromFunction((i: Int, j: Int) => if i == j then 1 else 0)
