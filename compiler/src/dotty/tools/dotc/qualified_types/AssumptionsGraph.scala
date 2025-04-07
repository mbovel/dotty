class AssumptionsGraph:
  enum Model:
    case Int(value: Option[Model], lowerBounds: List[Model], upperBounds: List[Model])
    case Boolean(value: Option[Model])
    case String(value: Option[Model])
    case Constructor(symbol: Symbol, args: List[Model])
    case Other(tp: Type)

    override def hashCode() = System.identityHashCode(this)
    override def equals(that: Any) = this eq that

  enum Alias:
    case Var(symbol: Symbol)
    case Call(symbol: Model, args: List[Model])

  val aliasesOf = mutable.Map[Model, Set[Alias]]
  val modelOf = mutable.Map[Alias, Model]
  val appearsIn = mutable.Map[Model, Set[Alias]]
