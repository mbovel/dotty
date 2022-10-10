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
  val vNPlus1 = Vec(nPlus1)
  vNPlus1.zip(Vec(n + 1))
  //             ^^^^^^^^^^^
  //             Found:    Vec((nPlus1 : Int))
  //             Required: Vec((?1 : Int))
  //             where:    ?1 is an unknown value of type Int

dependent def letAbstractionDependent =
  val n: Int = ???
  val nPlus1 /*: n.type + 1*/ = n + 1
  val vNPlus1 = Vec(nPlus1)
  vNPlus1.zip(Vec(n + 1))
