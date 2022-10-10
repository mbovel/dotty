import scala.compiletime.ops.int.*

dependent class Vec(val size: Int):
  dependent def ++(v2: Vec) = Vec(size + v2.size)
  def zip(v2: Vec(size.type)) = Vec(size)

def vecSipExample =
  val v1 = Vec(1)
  val v1T: Vec(1) = v1
  val v2 = Vec(2)
  val v2T: Vec(2) = v2
  val v3 = v1T ++ v2T
  val v3T: Vec(3) = v3
  val one = 1
  val vOne = Vec(one)
  val vOneT: Vec(one.type) = vOne
  val vTwo = Vec(one + 1)
  val vTwoT: Vec = vTwo
  val vThree = vOneT ++ vTwoT
  val vThreeT: Vec = vThree

def letAbstractionNormal =
  val n: Int = ???
  val nPlus1 /*: Int*/ = n + 1
  Vec(nPlus1).zip(Vec(n + 1))
  //             ^^^^^^^^^^^
  //             Found:    Vec((nPlus1 : Int))
  //             Required: Vec((?1 : Int))
  //             where:    ?1 is an unknown value of type Int

dependent def letAbstractionDependent =
  val n: Int = ???
  val nPlus1 /*: n.type + 1*/ = n + 1
  Vec(nPlus1).zip(Vec(n + 1))
  //              ^^^^^^^^^^
  //              Found:    Vec(((n : Int) + (1 : Int)))
  //              Required: Vec((nPlus1 : (n : Int) + (1 : Int)))

dependent case class Tensor(dims: Tuple):
  dependent def size =
    dependent def loop(dims: Tuple): Int =
      dims match
        case EmptyTuple => 0
        case (name: String, value: Int) *: tail => value * loop(tail)
    loop(dims)

def s: String = ???
val test: 100 = Tensor((("x", 10), (s, 10))).size

dependent case class CompiletimeMap(items: List[(Any, Any)]):
  dependent def updated(key: Any, value: Any) = CompiletimeMap((key, value) :: items)
  dependent def get(key: Any) =
    dependent def loop(items: List[(Any, Any)]): Option[Any] = items match
      case Nil => None
      case (k, v) :: tail =>
        if key == k then Some(v)
        else loop(tail)
    loop(items)

val cond: Boolean = ???
val test2: Some["a" | "b"] =
  CompiletimeMap(Nil)
    .updated(1, "a").updated(2, "b").updated(3, "c")
    .get(if cond then 1 else 2)



