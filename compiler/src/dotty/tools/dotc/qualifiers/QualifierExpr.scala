package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, Names.*, ast.tpd.*
import StdNames.nme
import util.Property.Key

import scala.collection.mutable
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.Text
import dotty.tools.dotc.core.Constants.Constant

import math.Ordering.Implicits.seqOrdering
import scala.annotation.threadUnsafe

enum QualifierExpr:
  import QualifierExpr.*

  // Predicates:
  case Var(i: Int)
  case True
  case False
  case And(args: Set[QualifierExpr])
  case Or(args: Set[QualifierExpr])
  case Not(arg: QualifierExpr)
  case IfThenElse(cond: QualifierExpr, thn: QualifierExpr, els: QualifierExpr)
  case Equal(l: QualifierExpr, right: QualifierExpr)
  case LessThan(left: QualifierExpr, right: QualifierExpr)
  case IsInstanceOf(t: QualifierExpr, tp: QualifierExpr)
  case ForAll(lambda: QualifierExpr)
  case Exists(lambda: QualifierExpr)

  // General expressions:
  case Ref(index: Symbol)
  case Get(ref: QualifierExpr, prop: QualifierExpr)
  case App(fun: QualifierExpr, args: List[QualifierExpr])
  case Lambda(index: Int, body: QualifierExpr)
  case LambdaArg(lambdaIndex: Int, argIndex: Int)
  case IntSum(const: Int, args: List[QualifierExpr])
  case IntProduct(const: Int, args: List[QualifierExpr])
  case IntConst(value: Int)
  case DoubleConst(value: Double)
  case StringConst(value: String)

  override def toString(): String =
    def showApp(name: String, args: Iterable[QualifierExpr]): String =
      f"$name(${args.mkString(", ")})"
    this match
      case Var(i)                           => f"?$i"
      case True                             => "true"
      case False                            => "false"
      case And(args)                        => showApp("and", args)
      case Or(args)                         => showApp("or", args)
      case Not(arg)                         => f"not(${arg})"
      case IfThenElse(cond, thn, els)       => f"if ${cond} then ${thn} else ${els}"
      case Equal(left, right)               => f"${left} == ${right}"
      case LessThan(left, right)            => f"${left} < ${right}"
      case IsInstanceOf(t, tp)              => f"${t}.isInstanceOf($tp)"
      case ForAll(lambda)                   => f"forall($lambda)"
      case Exists(lambda)                   => f"exists($lambda)"
      case IntConst(value)                  => value.toString
      case DoubleConst(value)               => value.toString
      case StringConst(value)               => value.toString
      case Ref(sym)                         => f"${sym}"
      case Get(ref, prop)                   => f"${ref}[${prop}]"
      case App(fun, args)                   => showApp(fun.toString, args)
      case Lambda(index, body)              => f"x$index => ${body}"
      case LambdaArg(lambdaIndex, argIndex) => if this == predArg then "it" else f"x$lambdaIndex($argIndex)"
      case IntSum(const, args)              => showApp("sum", IntConst(const) :: args)
      case IntProduct(const, args)          => showApp("prod", IntConst(const) :: args)

  def map(f: QualifierExpr => QualifierExpr): QualifierExpr =
    this match
      case Var(_)                           => f(this)
      case True                             => f(this)
      case False                            => f(this)
      case And(args)                        => f(args.map(_.map(f)).foldLeft(True)(and))
      case Or(args)                         => f(args.map(_.map(f)).foldLeft(False)(or))
      case Not(arg)                         => f(not(arg.map(f)))
      case IfThenElse(cond, thn, els)       => f(ifThenElse(cond.map(f), thn.map(f), els.map(f)))
      case Equal(left, right)               => f(equal(left.map(f), right.map(f)))
      case LessThan(left, right)            => f(lessThan(left.map(f), right.map(f)))
      case IsInstanceOf(t, tp)              => f(IsInstanceOf(t.map(f), tp))
      case ForAll(lambda)                   => f(ForAll(lambda.map(f)))
      case Exists(lambda)                   => f(Exists(lambda.map(f)))
      case IntConst(_)                      => f(this)
      case DoubleConst(_)                   => f(this)
      case StringConst(_)                   => f(this)
      case Ref(sym)                         => f(this)
      case Get(ref, prop)                   => f(Get(ref.map(f), prop.map(f)))
      case App(fun, args)                   => f(App(fun.map(f), args.map(_.map(f))))
      case Lambda(index, body)              => f(Lambda(index, body.map(f)))
      case LambdaArg(lambdaIndex, argIndex) => f(this)
      case IntSum(const, args)              => f(args.map(_.map(f)).foldLeft(IntConst(const))(intSum))
      case IntProduct(const, args)          => f(args.map(_.map(f)).foldLeft(IntConst(const))(intProduct))

  // TODO(mbovel): optimize allocations. Ideally, map(identity) shouldn't
  // allocate anything. This would require optimized copy methods (aka .derived)
  // and collections .map. Worth it?

  def equiv(that: QualifierExpr): Boolean =
    this == that || this.normalized() == that.normalized()

  def normalized(): QualifierExpr =
    this.map(_.shallowNormalized())

  def approxVarsToTrue(): QualifierExpr =
    this.map {
      case _: Var => True
      case _      => this
    }

  @threadUnsafe lazy val hasVars: Boolean =
    this match
      case Var(i) => true
      case True => false
      case False => false
      case And(args) => args.exists(_.hasVars)
      case Or(args) => args.exists(_.hasVars)
      case Not(arg) => arg.hasVars
      case IfThenElse(cond, thn, els) => cond.hasVars || thn.hasVars || els.hasVars
      case Equal(l, right) => l.hasVars || right.hasVars
      case LessThan(left, right) => left.hasVars || right.hasVars
      case IsInstanceOf(t, tp) => t.hasVars || tp.hasVars
      case ForAll(lambda) => lambda.hasVars
      case Exists(lambda) => lambda.hasVars
      case Ref(index) => false
      case Get(ref, prop) => ref.hasVars || prop.hasVars
      case App(fun, args) => fun.hasVars || args.exists(_.hasVars)
      case Lambda(index, body) => body.hasVars
      case LambdaArg(lambdaIndex, argIndex) => false
      case IntSum(const, args) => args.exists(_.hasVars)
      case IntProduct(const, args) => args.exists(_.hasVars)
      case IntConst(value) => false
      case DoubleConst(value) => false
      case StringConst(value) => false


  @threadUnsafe lazy val vars: List[Var] =
    this match
      case v: Var => List(v)
      case True => Nil
      case False => Nil
      case And(args) => args.toList.flatMap(_.vars)
      case Or(args) => args.toList.flatMap(_.vars)
      case Not(arg) => arg.vars
      case IfThenElse(cond, thn, els) => cond.vars ++ thn.vars ++ els.vars
      case Equal(l, right) => l.vars ++ right.vars
      case LessThan(left, right) => left.vars ++ right.vars
      case IsInstanceOf(t, tp) => t.vars ++ tp.vars
      case ForAll(lambda) => lambda.vars
      case Exists(lambda) => lambda.vars
      case Ref(index) => Nil
      case Get(ref, prop) => ref.vars ++ prop.vars
      case App(fun, args) => fun.vars ++ args.flatMap(_.vars)
      case Lambda(index, body) => body.vars
      case LambdaArg(lambdaIndex, argIndex) => Nil
      case IntSum(const, args) => args.flatMap(_.vars)
      case IntProduct(const, args) => args.flatMap(_.vars)
      case IntConst(value) => Nil
      case DoubleConst(value) => Nil
      case StringConst(value) => Nil

  private def shallowNormalized(): QualifierExpr =
    this match
      case And(args) =>
        if args.exists(arg => args.contains(Not(arg))) then False
        else this
      case Or(args) =>
        if args.exists(arg => args.contains(Not(arg))) then True
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
            .sortBy(_.hashCode())
        if newConst == 0 && newArgs.length == 1 then newArgs.head
        else IntSum(groups.getOrElse(Set.empty, 0) + const, newArgs)
      case IntProduct(const, args) =>
        val newArgs = args.toList.sortBy(_.hashCode())
        IntProduct(const, newArgs)
      case _ =>
        this

object QualifierExpr:
  import QualifierExpr.*

  val predArg = LambdaArg(0, 0)

  def and(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (False, _) | (False, _)  => False
      case (_, True)                => l
      case (True, _)                => r
      case (And(lArgs), And(rArgs)) => And(lArgs ++ rArgs)
      case (And(lArgs), _)          => And(lArgs + r)
      case (_, And(rArgs))          => And(rArgs + l)
      case _                        => And(Set(l, r))

  def or(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (True, _) | (True, _)  => True
      case (_, False)             => l
      case (False, _)             => r
      case (Or(lArgs), Or(rArgs)) => Or(lArgs ++ rArgs)
      case (Or(lArgs), _)         => Or(lArgs + l)
      case (_, Or(rArgs))         => Or(rArgs + l)
      case _ if r == l            => l
      case _                      => Or(Set(l, r))

  def not(arg: QualifierExpr) =
    arg match
      case True      => False
      case False     => True
      case Not(arg1) => arg1
      case arg1      => Not(arg1)

  def ifThenElse(cond: QualifierExpr, thn: QualifierExpr, els: QualifierExpr) =
    cond match
      case True  => thn
      case False => els
      case _     => IfThenElse(cond, thn, els)

  def equal(l: QualifierExpr, r: QualifierExpr) =
    if l == r then True
    else if l.hashCode > r.hashCode then Equal(r, l)
    else Equal(l, r)

  def lessThan(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case (IntConst(x), IntConst(y)) => if x < y then True else False
      case _                          => LessThan(l, r)

  def intSum(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (IntSum(lC, lArgs), IntSum(rC, rArgs)) => IntSum(lC + rC, lArgs ++ rArgs)
      case (IntSum(lC, lArgs), IntConst(rC))      => IntSum(lC + rC, lArgs)
      case (IntSum(lC, lArgs), _)                 => IntSum(lC, r :: lArgs)
      case (IntConst(lC), IntSum(rC, rArgs))      => IntSum(lC + rC, rArgs)
      case (IntConst(lC), IntConst(rC))           => IntConst(lC + rC)
      case (IntConst(lC), _)                      => IntSum(lC, List(r))
      case (_, IntSum(rC, rArgs))                 => IntSum(rC, l :: rArgs)
      case (_, IntConst(rC))                      => IntSum(rC, List(l))
      case _                                      => IntSum(1, List(l, r))

  def intProduct(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (IntProduct(lC, lArgs), IntProduct(rC, rArgs)) => IntProduct(lC * rC, lArgs ++ rArgs)
      case (IntProduct(lC, lArgs), IntConst(rC))          => IntProduct(lC * rC, lArgs)
      case (IntProduct(lC, lArgs), _)                     => IntProduct(lC, r :: lArgs)
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

  private val cache = mutable.HashMap[Type, QualifierExpr]()

  def ofType(tp: Type)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    // cache.getOrElseUpdate(tp, computeOfType(tp))
    val res = computeOfType(tp)
    // println(f"ofType(${tp.show}) == ${res.show}")
    res

  private def computeOfType(tp: Type)(using Context): QualifierExpr =
    import QualifierExpr.*
    tp.dealias match
      case QualifiedType(parent, pred) => and(ofType(parent), pred)
      case AndType(tp1, tp2)           => and(ofType(tp1), ofType(tp2))
      case OrType(tp1, tp2) if tp1 frozen_=:= tp2            => or(ofType(tp1), ofType(tp2))
      case tp: TypeProxy               => ofType(tp.underlying)
      case _                           => True

  val intBinOps: Set[Name] =
    Set(nme.EQ, nme.NE, nme.GT, nme.GE, nme.LT, nme.LE, nme.ADD, nme.MINUS, nme.MUL, nme.DIV)

  def fromClosure(tree: Tree)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    tree match
      case closureDef(meth) =>
        val res = fromTree(meth.rhs)(using meth.paramss(0)(0).symbol)
        res
      case _ => throw new Error(f"Cannot translate ${tree}")

  def fromTree(tree: Tree)(using predArgSymbol: Symbol)(using Context): QualifierExpr =
    tree match
      case id: Ident =>
        if id.symbol == predArgSymbol then predArg
        else Ref(id.symbol)
      case Apply(fun, args) =>
        fun match
          case Select(qualifier, name)
              if intBinOps.contains(name) && qualifier.tpe <:< defn.IntType =>
            val lhs = fromTree(qualifier)
            val rhs = fromTree(args(0))
            name match
              case nme.EQ  => Equal(lhs, rhs)
              case nme.NE  => Not(Equal(lhs, rhs))
              case nme.GT  => and(Not(Equal(lhs, rhs)), Not(LessThan(lhs, rhs)))
              case nme.GE  => Not(LessThan(lhs, rhs))
              case nme.LT  => LessThan(lhs, rhs)
              case nme.LE  => and(Not(Equal(lhs, rhs)), LessThan(lhs, rhs))
              case nme.ADD => intSum(lhs, rhs)
              case nme.SUB => intSum(lhs, intNegate(rhs))
              case nme.MUL => intProduct(lhs, rhs)
          case _ =>
            App(fromTree(fun), args.map(fromTree))
      case Select(qualifier, name) =>
        Get(fromTree(qualifier), StringConst(name.toString))
      case Literal(c) if fromConst.isDefinedAt(c) =>
        fromConst(c)
      case _ =>
        throw new Error(f"Cannot translate ${tree}")

  val fromConst: PartialFunction[Constant, QualifierExpr] = {
    case Constant(value: Int)    => IntConst(value)
    case Constant(value: Double) => DoubleConst(value)
    case Constant(value: String) => StringConst(value)
  }
