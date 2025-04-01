

object SimpleExprs:
  private enum BinaryOp:
    case BooleanAnd
    case BooleanOr
    case BooleanEquals
    case IntMinus
  /*
  private enum SimpleExpr:
    case Atom(tp: SingletonType)
    case TypeApply(fun: Tree, args: List[Type])
    case Apply(fun: Tree, args: List[SimpleExpr])
    case Select(qualifier: SimpleExpr, name: Name, signature: Signature)
    case Let(body: SimpleExpr, tp: Type)
    case Closure(paramTps: List[Type], body: SimpleExpr)
    case BinaryOp(op: Name, lhs: SimpleExpr, rhs: SimpleExpr)
    case IntSum(c: Int, summands: List[SimpleExpr])
    case IntProduct(c: Int, factors: List[SimpleExpr])
  */
