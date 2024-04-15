package dotty.tools.dotc.qualifiers

import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.math.Ordering.Implicits.{seqOrdering, infixOrderingOps}

import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types.{ConstantType, NamedType, RecThis, SingletonType, SuperType, TermRef, TermParamRef, ThisType, Type}
import dotty.tools.dotc.printing.{Printer, Showable}
import dotty.tools.dotc.printing.Texts.Text

import QualifierExpr.*

type ReferenceType = TermRef | ThisType | SuperType | TermParamRef | RecThis

enum QualifierExpr extends Showable:
  // Predicates:
  case ApplyVar(i: Int, arg: QualifierExpr = PredArg)
  case True
  case False
  case And(args: List[QualifierExpr])
  case Or(args: List[QualifierExpr])
  case Not(arg: QualifierExpr)
  case Equal(left: QualifierExpr, right: QualifierExpr)
  case NotEqual(left: QualifierExpr, right: QualifierExpr)
  case Less(left: QualifierExpr, right: QualifierExpr)
  case LessEqual(left: QualifierExpr, right: QualifierExpr)
  case Greater(left: QualifierExpr, right: QualifierExpr)
  case GreaterEqual(left: QualifierExpr, right: QualifierExpr)

  // General expressions:
  case PredArg
  case Ref(tp: ReferenceType)
  case Get(prefix: QualifierExpr, name: Name)
  case App(fun: QualifierExpr, args: List[QualifierExpr])
  case IntSum(const: Int, args: List[QualifierExpr])
  case IntProduct(const: Int, args: List[QualifierExpr])
  case IntConst(value: Int)
  case DoubleConst(value: Double)
  case StringConst(value: String)

  def toText(printer: Printer): Text = printer.toTextQualifierExpr(this)

  def map(f: QualifierExpr => QualifierExpr): QualifierExpr =
    // TODO(mbovel): optimize allocations. `map(x => x)` shouldn't allocate
    // anything. This would require 1. optimized copy methods (aka .derived) on
    // `QualifierExpr` and 2. optimized collection mapping, for example using
    // using `.mapConserve` instead of `.map`. Should benchmark.
    this match
      case ApplyVar(i, arg)          => f(ApplyVar(i, arg.map(f)))
      case True                      => f(this)
      case False                     => f(this)
      case And(args)                 => f(args.map(_.map(f)).foldLeft(True)(and))
      case Or(args)                  => f(args.map(_.map(f)).foldLeft(False)(or))
      case Not(arg)                  => f(not(arg.map(f)))
      case Equal(left, right)        => f(equal(left.map(f), right.map(f)))
      case NotEqual(left, right)     => f(notEqual(left.map(f), right.map(f)))
      case Less(left, right)         => f(less(left.map(f), right.map(f)))
      case LessEqual(left, right)    => f(lessEqual(left.map(f), right.map(f)))
      case Greater(left, right)      => f(greater(left.map(f), right.map(f)))
      case GreaterEqual(left, right) => f(greaterEqual(left.map(f), right.map(f)))
      case IntConst(value)           => f(this)
      case DoubleConst(value)        => f(this)
      case StringConst(value)        => f(this)
      case PredArg                   => f(this)
      case Ref(tp)                   => f(this)
      case Get(prefix, name)         => f(Get(f(prefix), name))
      case App(fun, args)            => f(App(fun.map(f), args.map(_.map(f))))
      case IntSum(const, args)       => f(args.map(_.map(f)).foldRight(IntConst(const))(intSum))
      case IntProduct(const, args)   => f(args.map(_.map(f)).foldRight(IntConst(const))(intProduct))

  def foreach(f: QualifierExpr => Unit): Unit =
    f(this)
    this match
      case And(args)                 => args.foreach(_.foreach(f))
      case Or(args)                  => args.foreach(_.foreach(f))
      case Not(arg)                  => arg.foreach(f)
      case Equal(left, right)        => left.foreach(f); right.foreach(f)
      case NotEqual(left, right)     => left.foreach(f); right.foreach(f)
      case Less(left, right)         => left.foreach(f); right.foreach(f)
      case LessEqual(left, right)    => left.foreach(f); right.foreach(f)
      case Greater(left, right)      => left.foreach(f); right.foreach(f)
      case GreaterEqual(left, right) => left.foreach(f); right.foreach(f)
      case Get(prefix, name)         => prefix.foreach(f)
      case App(fun, args)            => fun.foreach(f); args.foreach(_.foreach(f))
      case IntSum(const, args)       => args.foreach(_.foreach(f))
      case IntProduct(const, args)   => args.foreach(_.foreach(f))
      case _                         => ()

  def equiv(that: QualifierExpr): Boolean =
    // Used to also check if `this.map(_.shallowNormalize()) ==
    // that.map(_.shallowNormalize())`. Now, it assumes that the caller has
    // already normalized the expressions.
    this == that

  def subst(from: QualifierExpr, to: QualifierExpr): QualifierExpr =
    this.map:
      case `from` => to
      case x      => x

  @threadUnsafe lazy val vars: List[ApplyVar] =
    this match
      case v: ApplyVar               => List(v)
      case True                      => Nil
      case False                     => Nil
      case And(args)                 => args.toList.flatMap(_.vars)
      case Or(args)                  => args.toList.flatMap(_.vars)
      case Not(arg)                  => arg.vars
      case Equal(left, right)        => left.vars ++ right.vars
      case NotEqual(left, right)     => left.vars ++ right.vars
      case Less(left, right)         => left.vars ++ right.vars
      case LessEqual(left, right)    => left.vars ++ right.vars
      case Greater(left, right)      => left.vars ++ right.vars
      case GreaterEqual(left, right) => left.vars ++ right.vars
      case Get(prefix, name)         => prefix.vars
      case App(fun, args)            => fun.vars ++ args.flatMap(_.vars)
      case IntSum(const, args)       => args.flatMap(_.vars)
      case IntProduct(const, args)   => args.flatMap(_.vars)
      case _                         => Nil

  def hasVars: Boolean = vars.nonEmpty

  def shallowNormalize(): QualifierExpr =
    def equalNorm(left: QualifierExpr, right: QualifierExpr) = equal(left, right).shallowNormalize()
    this match
      case And(args) if args.exists(arg => args.contains(Not(arg))) =>
        False
      case Or(args) if args.exists(arg => args.contains(Not(arg))) =>
        True
      case Equal(left, right) if right < left =>
        Equal(right, left)
      case NotEqual(left, right)  => not(equalNorm(left, right))
      case Less(left, right)      => and(not(equalNorm(left, right)), greaterEqual(left, right))
      case LessEqual(left, right) => not(and(not(Equal(left, right)), greaterEqual(right, left)))
      case Greater(left, right)   => not(greaterEqual(left, right))
      case IntSum(const, args)    =>
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
            .map((args, c) => IntProduct(c, args.toList.sorted))
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

  type Const = IntConst | DoubleConst | StringConst

  def topAnd(expr: QualifierExpr): And =
    expr match
      case expr: And => expr
      case _         => And(expr :: Nil)

  def and(exprs: QualifierExpr*): QualifierExpr = exprs.foldLeft(True)(and)

  def and(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (False, _) | (False, _)  => False
      case (_, True)                => l
      case (True, _)                => r
      case (And(lArgs), And(rArgs)) => And(lArgs ++ rArgs)
      case (And(lArgs), _)          => And(lArgs :+ r) // TODO(mbovel): warning: inefficient
      case (_, And(rArgs))          => And(l :: rArgs)
      case _                        => And(List(l, r))

  def or(exprs: QualifierExpr*): QualifierExpr = exprs.foldLeft(False)(or)

  def or(l: QualifierExpr, r: QualifierExpr): QualifierExpr =
    (l, r) match
      case (True, _) | (True, _)  => True
      case (_, False)             => l
      case (False, _)             => r
      case (Or(lArgs), Or(rArgs)) => Or(lArgs ++ rArgs)
      case (Or(lArgs), _)         => Or(lArgs :+ r)
      case (_, Or(rArgs))         => Or(l :: rArgs)
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
      case _ if l == r                      => True
      case (IntConst(x), IntConst(y))       => if x == y then True else False
      case (DoubleConst(x), DoubleConst(y)) => if x == y then True else False
      case (StringConst(x), StringConst(y)) => if x == y then True else False
      case _                                => Equal(l, r)

  def notEqual(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case _ if l == r                      => False
      case (IntConst(x), IntConst(y))       => if x != y then True else False
      case (DoubleConst(x), DoubleConst(y)) => if x != y then True else False
      case (StringConst(x), StringConst(y)) => if x != y then True else False
      case _                                => NotEqual(l, r)

  def less(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case (IntConst(x), IntConst(y))       => if x < y then True else False
      case (DoubleConst(x), DoubleConst(y)) => if x < y then True else False
      case _                                => Less(l, r)

  def lessEqual(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case (IntConst(x), IntConst(y))       => if x <= y then True else False
      case (DoubleConst(x), DoubleConst(y)) => if x <= y then True else False
      case _                                => LessEqual(l, r)

  def greater(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case (IntConst(x), IntConst(y))       => if x > y then True else False
      case (DoubleConst(x), DoubleConst(y)) => if x > y then True else False
      case _                                => Greater(l, r)

  def greaterEqual(l: QualifierExpr, r: QualifierExpr) =
    (l, r) match
      case (IntConst(x), IntConst(y))       => if x >= y then True else False
      case (DoubleConst(x), DoubleConst(y)) => if x >= y then True else False
      case _                                => GreaterEqual(l, r)

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

  def get(x: QualifierExpr, name: Name)(using Context): QualifierExpr =
    x match
      case Ref(tp: TermRef) => Ref(tp.select(name.asTermName))
      case _                => Get(x, name)

  /** Spaceship operator */
  extension (x: Int) inline def <=>(inline y: Int) = if x != 0 then x else y

  given ordering: Ordering[QualifierExpr] =
    import math.Ordered.orderingToOrdered
    (a, b) =>
      (a, b) match
        case (True, True)                                               => 0
        case (True, _)                                                  => -1
        case (_, True)                                                  => 1
        case (False, False)                                             => 0
        case (False, _)                                                 => -1
        case (_, False)                                                 => 1
        case (Not(x), Not(y))                                           => x.compareTo(y)
        case (Not(_), _)                                                => -1
        case (_, Not(_))                                                => 1
        case (IntConst(x), IntConst(y))                                 => x.compareTo(y)
        case (IntConst(_), _)                                           => -1
        case (_, IntConst(_))                                           => 1
        case (DoubleConst(x), DoubleConst(y))                           => x.compareTo(y)
        case (DoubleConst(_), _)                                        => -1
        case (_, DoubleConst(_))                                        => 1
        case (StringConst(x), StringConst(y))                           => x.compareTo(y)
        case (StringConst(_), _)                                        => -1
        case (_, StringConst(_))                                        => 1
        case (IntSum(xConst, xArgs), IntSum(yConst, yArgs))             => xConst.compareTo(yConst) <=> xArgs.compareTo(yArgs)
        case (IntSum(_, _), _)                                          => -1
        case (_, IntSum(_, _))                                          => 1
        case (IntProduct(xConst, xArgs), IntProduct(yConst, yArgs))     => xConst.compareTo(yConst) <=> xArgs.compareTo(yArgs)
        case (IntProduct(_, _), _)                                      => -1
        case (_, IntProduct(_, _))                                      => 1
        case (And(xArgs), And(yArgs))                                   => xArgs.compareTo(yArgs)
        case (And(_), _)                                                => -1
        case (_, And(_))                                                => 1
        case (Or(xArgs), Or(yArgs))                                     => xArgs.compareTo(yArgs)
        case (Or(_), _)                                                 => -1
        case (_, Or(_))                                                 => 1
        case (Equal(xLeft, xRight), Equal(yLeft, yRight))               => xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (Equal(_, _), _)                                           => -1
        case (_, Equal(_, _))                                           => 1
        case (NotEqual(xLeft, xRight), NotEqual(yLeft, yRight))         => xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (NotEqual(_, _), _)                                        => -1
        case (_, NotEqual(_, _))                                        => 1
        case (Less(xLeft, xRight), Less(yLeft, yRight))                 => xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (Less(_, _), _)                                            => -1
        case (_, Less(_, _))                                            => 1
        case (LessEqual(xLeft, xRight), LessEqual(yLeft, yRight))       => xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (LessEqual(_, _), _)                                       => -1
        case (_, LessEqual(_, _))                                       => 1
        case (Greater(xLeft, xRight), Greater(yLeft, yRight))           => xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (Greater(_, _), _)                                         => -1
        case (_, Greater(_, _))                                         => 1
        case (GreaterEqual(xLeft, xRight), GreaterEqual(yLeft, yRight)) => xLeft.compareTo(yLeft) <=> xRight.compareTo(yRight)
        case (GreaterEqual(_, _), _)                                    => -1
        case (_, GreaterEqual(_, _))                                    => 1
        case (Get(xPrefix, xName), Get(yPrefix, yName)) =>
          xPrefix.compareTo(yPrefix) <=> System.identityHashCode(xName).compareTo(System.identityHashCode(yName))
        case (Get(_, _), _)                         => -1
        case (_, Get(_, _))                         => 1
        case (App(xFun, xArgs), App(yFun, yArgs))   => xFun.compareTo(yFun) <=> xArgs.compareTo(yArgs)
        case (App(_, _), _)                         => -1
        case (_, App(_, _))                         => 1
        case (ApplyVar(x, xArg), ApplyVar(y, yArg)) => x.compareTo(y) <=> xArg.compareTo(yArg)
        case (ApplyVar(_, _), _)                    => -1
        case (_, ApplyVar(_, _))                    => 1
        case (PredArg, PredArg)                     => 0
        case (PredArg, _)                           => -1
        case (_, PredArg)                           => 1
        case (Ref(x), Ref(y))                       => System.identityHashCode(x).compareTo(System.identityHashCode(y))
