type Index = {x: Int with x >= 0}
type LongIndex = {x: Long with x >= 0}

trait Matrix[T]:
  def lastRow: Index
  def lastColumn: Index
  type RowIndex = {i: Index with i <= lastRow}
  type ColumnIndex = {i: Index with i <= lastColumn}
  def apply(i: RowIndex, j: ColumnIndex): T

def printMatrix[T](pr: T => String, m: Matrix[T]): Unit =
  for i <- 0 to m.lastRow do
    for j <- 0 to m.lastColumn do
      print(pr(m(i,j)))
    println("")

def fromFunction[T](lastR: Index, lastC: Index,
                    f : ({i: Int with i <= lastR}, {j: Int with j <= lastC}) => T): Matrix[T] =
  new Matrix {
    def lastRow = lastR
    def lastColumn = lastC
    def apply(i: RowIndex, j: ColumnIndex) = f(i, j)
  }

def diagonal(i: Index): Matrix[Int] =
  fromFunction(i, i, (i: Int, j: Int) => if i == j then 1 else 0)

@main
def test: Unit =
  printMatrix((x:Int) => x.toString, diagonal(4))
