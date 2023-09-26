package dotty.tools.dotc.qualifiers

import scala.annotation.threadUnsafe
import scala.collection.mutable
import math.Ordering.Implicits.{seqOrdering, infixOrderingOps}

enum QualifierExpr:
  import QualifierExpr.*

  // Predicates:
  case ApplyVar(i: Int, arg: QualifierExpr = PredArg)
  case True
  case False
  case And(args: List[QualifierExpr])
  case Or(args: List[QualifierExpr])
  case Not(arg: QualifierExpr)
  case Equal(left: QualifierExpr, right: QualifierExpr)
  case LessThan(left: QualifierExpr, right: QualifierExpr)

  // General expressions:
  case PredArg
  case Ref(id: Int, name: String)
  case App(fun: QualifierExpr, args: List[QualifierExpr])
  case Lambda(params: List[Ref], body: QualifierExpr)
  case IntSum(const: Int, args: List[QualifierExpr])
  case IntProduct(const: Int, args: List[QualifierExpr])
  case IntConst(value: Int)
  case DoubleConst(value: Double)
  case StringConst(value: String)

  override def toString(): String =
    def showApp(name: String, args: Iterable[QualifierExpr]): String = f"$name(${args.mkString(", ")})"
    this match
      case ApplyVar(i, arg)                  => f"?$i($arg)"
      case True                    => "true"
      case False                   => "false"
      case And(args)               => showApp("and", args)
      case Or(args)                => showApp("or", args)
      case Not(arg)                => f"not(${arg})"
      case Equal(left, right)      => f"${left} == ${right}"
      case LessThan(left, right)   => f"${left} < ${right}"
      case IntConst(value)         => value.toString
      case DoubleConst(value)      => value.toString
      case StringConst(value)      => value.toString
      case PredArg                 => "it"
      case Ref(n, name)            => name
      case App(fun, args)          => showApp(fun.toString, args)
      case Lambda(params, body)    => f"(${params.mkString(",")}) => ${body}"
      case IntSum(const, args)     => showApp("sum", IntConst(const) :: args)
      case IntProduct(const, args) => showApp("prod", IntConst(const) :: args)

  def map(f: QualifierExpr => QualifierExpr): QualifierExpr =
    // TODO(mbovel): optimize allocations. `map(x => x)` shouldn't allocate
    // anything. This would require optimized copy methods (aka .derived) on
    // `QualifierExpr` and collection mapping, for example using using
    // `.mapConserve` instead of `.map`. Should benchmark.
    this match
      case ApplyVar(i, arg)                  => f(ApplyVar(i, arg.map(f)))
      case True                    => f(this)
      case False                   => f(this)
      case And(args)               => f(args.map(_.map(f)).foldLeft(True)(and))
      case Or(args)                => f(args.map(_.map(f)).foldLeft(False)(or))
      case Not(arg)                => f(not(arg.map(f)))
      case Equal(left, right)      => f(equal(left.map(f), right.map(f)))
      case LessThan(left, right)   => f(lessThan(left.map(f), right.map(f)))
      case IntConst(value)         => f(this)
      case DoubleConst(value)      => f(this)
      case StringConst(value)      => f(this)
      case PredArg                 => f(this)
      case Ref(id, name)           => f(this)
      case App(fun, args)          => f(App(fun.map(f), args.map(_.map(f))))
      case Lambda(params, body)    => f(Lambda(params, body.map(f)))
      case IntSum(const, args)     => f(args.map(_.map(f)).foldRight(IntConst(const))(intSum))
      case IntProduct(const, args) => f(args.map(_.map(f)).foldRight(IntConst(const))(intProduct))


  def foreach(f: QualifierExpr => Unit): Unit =
    f(this)
    this match
      case And(args)               => args.foreach(_.foreach(f))
      case Or(args)                => args.foreach(_.foreach(f))
      case Not(arg)                => arg.foreach(f)
      case Equal(left, right)      => left.foreach(f); right.foreach(f)
      case LessThan(left, right)   => left.foreach(f); right.foreach(f)
      case App(fun, args)          => fun.foreach(f); args.foreach(_.foreach(f))
      case Lambda(params, body)    => body.foreach(f)
      case IntSum(const, args)     => args.foreach(_.foreach(f))
      case IntProduct(const, args) => args.foreach(_.foreach(f))
      case _                       => ()

  def equiv(that: QualifierExpr): Boolean =
    // Used to also check if `this.map(_.shallowNormalize()) ==
    // that.map(_.shallowNormalize())`. Now, it assumes that the caller has
    // already normalized the expressions.
    this == that

  def approxVarsToTrue(): QualifierExpr =
    this.map {
      case _: ApplyVar => True
      case _      => this
    }

  def subst(from: QualifierExpr, to: QualifierExpr): QualifierExpr =
    this.map {
      case `from` => to
      case x      => x
    }

  @threadUnsafe lazy val hasVars: Boolean =
    this match
      case ApplyVar(i, arg)                  => true
      case True                    => false
      case False                   => false
      case And(args)               => args.exists(_.hasVars)
      case Or(args)                => args.exists(_.hasVars)
      case Not(arg)                => arg.hasVars
      case Equal(l, right)         => l.hasVars || right.hasVars
      case LessThan(left, right)   => left.hasVars || right.hasVars
      case PredArg                 => false
      case Ref(id, name)           => false
      case App(fun, args)          => fun.hasVars || args.exists(_.hasVars)
      case Lambda(params, body)    => body.hasVars
      case IntSum(const, args)     => args.exists(_.hasVars)
      case IntProduct(const, args) => args.exists(_.hasVars)
      case IntConst(value)         => false
      case DoubleConst(value)      => false
      case StringConst(value)      => false

  @threadUnsafe lazy val vars: List[ApplyVar] =
    this match
      case v: ApplyVar                  => List(v)
      case True                    => Nil
      case False                   => Nil
      case And(args)               => args.toList.flatMap(_.vars)
      case Or(args)                => args.toList.flatMap(_.vars)
      case Not(arg)                => arg.vars
      case Equal(l, right)         => l.vars ++ right.vars
      case LessThan(left, right)   => left.vars ++ right.vars
      case PredArg                 => Nil
      case Ref(id, name)           => Nil
      case App(fun, args)          => fun.vars ++ args.flatMap(_.vars)
      case Lambda(params, body)    => body.vars
      case IntSum(const, args)     => args.flatMap(_.vars)
      case IntProduct(const, args) => args.flatMap(_.vars)
      case IntConst(value)         => Nil
      case DoubleConst(value)      => Nil
      case StringConst(value)      => Nil

  def shallowNormalize(): QualifierExpr =
    this match
      case And(args) =>
        if args.exists(arg => args.contains(Not(arg))) then False
        else this
      case Or(args) =>
        if args.exists(arg => args.contains(Not(arg))) then True
        else this
      case Equal(left, right) =>
        if right < left then Equal(right, left)
        else this
      case LessThan(left, right) =>
        if right < left then LessThan(right, left)
        else this
      case IntSum(const, args) =>
        /*
        // Stable sort implementation.
        val groups = collection.mutable.LinkedHashMap[Set[QualifierExpr], Int]()
        for arg <- args do
          val (key, value) = arg match
            case IntProduct(const, args) => (args.toSet, const)
            case IntConst(const)         => (Set.empty, const)
            case arg                     => (Set(arg), 1)
          val prev = groups.getOrElse(key, 0)
          groups.update(key, prev + value)
         */
        val groups =
          args.groupMapReduce {
            case IntProduct(const, args) => args.toSet
            case IntConst(const)         => Set.empty
            case arg                     => Set(arg)
          } {
            case IntProduct(const, args) => const
            case IntConst(const)         => const
            case arg                     => 1
          } {
            _ + _
          }
        val newConst = groups.getOrElse(Set.empty, 0) + const
        val newArgs =
          groups
            .filter((args, c) => c != 0 && !args.isEmpty)
            .map((args, c) => IntProduct(c, args.toList.sortBy(_.hashCode())))
            .toList
            .sorted
        if newConst == 0 && newArgs.length == 1 then newArgs.head
        else IntSum(groups.getOrElse(Set.empty, 0) + const, newArgs)
      case IntProduct(const, args) =>
        val newArgs = args.toList.sorted
        IntProduct(const, newArgs)
      case _ =>
        this

object QualifierExpr:
  import QualifierExpr.*

  type Const = IntConst | DoubleConst | StringConst

  def topAnd(expr: QualifierExpr): And =
    expr match
      case expr: And => expr
      case _         => And(expr :: Nil)

  def and(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (False, _) | (False, _)  => False
      case (_, True)                => l
      case (True, _)                => r
      case (And(lArgs), And(rArgs)) => And(lArgs ++ rArgs)
      case (And(lArgs), _)          => And(lArgs :+ r) // TODO(mbovel): warning: inefficient
      case (_, And(rArgs))          => And(rArgs :+ l) // TODO(mbovel): wrong order
      case _                        => And(List(l, r))

  def or(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (True, _) | (True, _)  => True
      case (_, False)             => l
      case (False, _)             => r
      case (Or(lArgs), Or(rArgs)) => Or(lArgs ++ rArgs)
      case (Or(lArgs), _)         => Or(lArgs :+ l)
      case (_, Or(rArgs))         => Or(rArgs :+ l)
      case _ if r == l            => l
      case _                      => Or(List(l, r))

  def not(arg: QualifierExpr) =
    arg match
      case True      => False
      case False     => True
      case Not(arg1) => arg1
      case arg1      => Not(arg1)

  def equal(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case _ if l == r => True
      case (l, True)   => l
      case (True, r) => r
      case (l, False) => not(l)
      case (False, r) => not(r)
      case _ => Equal(l, r)

  def lessThan(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case (IntConst(x), IntConst(y)) => if x < y then True else False
      case _ if l == r                => False
      case _ => LessThan(l, r)

  def intSum(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (IntSum(lC, lArgs), IntSum(rC, rArgs)) => IntSum(lC + rC, lArgs ++ rArgs)
      case (IntSum(lC, lArgs), IntConst(rC))      => IntSum(lC + rC, lArgs)
      case (IntSum(lC, lArgs), _)                 => IntSum(lC, lArgs :+ r)
      case (IntConst(lC), IntSum(rC, rArgs))      => IntSum(lC + rC, rArgs)
      case (IntConst(lC), IntConst(rC))           => IntConst(lC + rC)
      case (IntConst(lC), _)                      => IntSum(lC, List(r))
      case (_, IntSum(rC, rArgs))                 => IntSum(rC, l :: rArgs)
      case (_, IntConst(rC))                      => IntSum(rC, List(l))
      case _                                      => IntSum(0, List(l, r))

  def intProduct(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (IntProduct(lC, lArgs), IntProduct(rC, rArgs)) => IntProduct(lC * rC, lArgs ++ rArgs)
      case (IntProduct(lC, lArgs), IntConst(rC))          => IntProduct(lC * rC, lArgs)
      case (IntProduct(lC, lArgs), _)                     => IntProduct(lC, lArgs :+ r)
      case (IntConst(lC), IntProduct(rC, rArgs))          => IntProduct(lC * rC, rArgs)
      case (IntConst(lC), IntConst(rC))                   => IntConst(lC * rC)
      case (IntConst(lC), _)                              => IntProduct(lC, List(r))
      case (_, IntProduct(rC, rArgs))                     => IntProduct(rC, l :: rArgs)
      case (_, IntConst(rC))                              => IntProduct(rC, List(l))
      case _                                              => IntProduct(1, List(l, r))

  def intNegate(x: QualifierExpr): QualifierExpr =
    x match
      case IntProduct(c, args) => IntProduct(-c, args)
      case IntConst(c)         => IntConst(-c)
      case _                   => IntProduct(-1, List(x))

  extension (x: Int)
    // Spaceship operator
    inline def <=>(inline y: Int) =
      if x != 0 then x else y

  given ordering: Ordering[QualifierExpr] =
    import math.Ordered.orderingToOrdered
    (a, b) =>
      (a, b) match
        case (True, True) => 0
        case (True, _) => -1
        case (_, True) => 1
        case (False, False) => 0
        case (False, _) => -1
        case (_, False) => 1
        case (Not(x), Not(y)) => x.compareTo(y)
        case (Not(_), _) => -1
        case (_, Not(_)) => 1
        case (IntConst(x), IntConst(y)) => x.compareTo(y)
        case (IntConst(_), _) => -1
        case (_, IntConst(_)) => 1
        case (DoubleConst(x), DoubleConst(y)) => x.compareTo(y)
        case (DoubleConst(_), _) => -1
        case (_, DoubleConst(_)) => 1
        case (StringConst(x), StringConst(y)) => x.compareTo(y)
        case (StringConst(_), _) => -1
        case (_, StringConst(_)) => 1
        case (IntSum(xConst, xArgs), IntSum(yConst, yArgs)) =>
          xConst.compareTo(yConst) <=> xArgs.compareTo(yArgs)
        case (IntSum(_, _), _) => -1
        case (_, IntSum(_, _)) => 1
        case (IntProduct(xConst, xArgs), IntProduct(yConst, yArgs)) =>
          xConst.compareTo(yConst) <=> xArgs.compareTo(yArgs)
        case (IntProduct(_, _), _) => -1
        case (_, IntProduct(_, _)) => 1
        case (And(xArgs), And(yArgs)) => xArgs.compareTo(yArgs)
        case (And(_), _) => -1
        case (_, And(_)) => 1
        case (Or(xArgs), Or(yArgs)) => xArgs.compareTo(yArgs)
        case (Or(_), _) => -1
        case (_, Or(_)) => 1
        case (Equal(xLeft, xRight), Equal(yLeft, yRight)) =>
          xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (Equal(_, _), _) => -1
        case (_, Equal(_, _)) => 1
        case (LessThan(xLeft, xRight), LessThan(yLeft, yRight)) =>
          xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (LessThan(_, _), _) => -1
        case (_, LessThan(_, _)) => 1
        case (App(xFun, xArgs), App(yFun, yArgs)) =>
          xFun.compareTo(yFun) <=> xArgs.compareTo(yArgs)
        case (App(_, _), _) => -1
        case (_, App(_, _)) => 1
        case (ApplyVar(x, xArg), ApplyVar(y, yArg)) =>
          x.compareTo(y) <=> xArg.compareTo(yArg)
        case (ApplyVar(_, _), _) => -1
        case (_, ApplyVar(_, _)) => 1
        case (PredArg, PredArg) => 0
        case (PredArg, _) => -1
        case (_, PredArg) => 1
        case (Ref(id, name), Ref(id2, name2)) => id.compareTo(id2)
        case (Ref(_, _), _) => -1
        case (_, Ref(_, _)) => 1
        case (Lambda(xParams, xBody), Lambda(yParams, yBody)) =>
          xParams.compareTo(yParams) <=> xBody.compareTo(yBody)
