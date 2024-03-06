// TODO(Valentin889):
// 1. move this extension method to the Scala 3 lib.
//   - add it somewhere in /library/src
//   - reference it from Definitions.scala (see TailrecAnnot as an example)
// 2. special-case CheckQualifiedTypes to not throw a type error when the actual
//    type of `x.runtimeChecked` does not conform to the expected type. This
//    should probably be done in the method checkConformsExpr that should be
//    overridden in CheckQualifiedTypes. There, you should match `tree` to check
//    if it is an application of `runtimeChecked`. Something like:
//
//     tree match case Apply(fun, ...) if fun.symbol == defn.runtimeChecked =>
//          ...

extension (x: Any) def runtimeChecked: x.type = x

def main =
  val x: Int = ???
  val y: {v: Int with v == 10} = x.runtimeChecked
  //  val y: Int @qualified[Int](v => v == 10) = x.runtimeChecked[{v: Int with v == 10}]
