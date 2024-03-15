package dotty.tools
package dotc
package qualifiers

import scala.collection.mutable

import core.*, Types.*, Symbols.*, Contexts.*, Names.*, ast.tpd.*
import StdNames.nme
import Constants.Constant

import QualifierLogging.{trace, startTrace, endTrace, LogEvent, LogEventNode}
import QualifierExpr.*

object QualifierExprs:
  def fromType(tp: Type)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    trace[QualifierExpr](res => LogEvent.OfType(tp.show, res.show)):
      tp.dealias match
        case QualifiedType(parent, pred) => and(pred, fromType(parent))
        case AndType(tp1, tp2)           => and(fromType(tp1), fromType(tp2))
        case OrType(tp1, tp2)            => if tp1.widen frozen_=:= tp2.widen then or(fromType(tp1), fromType(tp2)) else True
        case ConstantType(value)         => equal(PredArg, fromConst(value))
        case tp: SingletonType           => and(equal(PredArg, Ref(tp)), fromType(tp.underlying))
        case tp: TypeProxy               => fromType(tp.underlying)
        case _                           => True

  val intBinOps: Set[Name] = Set(nme.EQ, nme.NE, nme.GT, nme.GE, nme.LT, nme.LE, nme.ADD, nme.MINUS, nme.MUL, nme.DIV)
  val boolBinsOps: Set[Name] = Set(nme.EQ, nme.NE, nme.ZAND, nme.ZOR)

  def fromClosure(tree: Tree)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    tree match
      case closureDef(meth) => fromTree(meth.rhs)(using meth.paramss(0)(0).symbol)
      case _                => throw new Error(f"Cannot translate ${tree}")

  def fromTree(tree: Tree)(using predArgSymbol: Symbol)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    trace[QualifierExpr](res => LogEvent.FromTree(tree.show, predArgSymbol.show, res.show)):
      tree match
        case id: Ident =>
          if id.symbol == predArgSymbol then PredArg
          else Ref(id.symbol.termRef)
        case Apply(fun, args) =>
          fun match
            case Select(qualifier, name)
                if boolBinsOps.contains(name) && (qualifier.tpe frozen_<:< defn.BooleanType) =>
              val lhs = fromTree(qualifier)
              val rhs = fromTree(args(0))
              name match
                case nme.EQ   => Equal(lhs, rhs)
                case nme.NE   => Not(Equal(lhs, rhs))
                case nme.ZAND => and(lhs, rhs)
                case nme.ZOR  => or(lhs, rhs)
            case Select(qualifier, name)
                if intBinOps.contains(name) && (qualifier.tpe frozen_<:< defn.IntType) =>
              val lhs = fromTree(qualifier)
              val rhs = fromTree(args(0))
              name match
                case nme.EQ  => equal(lhs, rhs)
                case nme.NE  => notEqual(lhs, rhs)
                case nme.GT  => greater(lhs, rhs)
                case nme.GE  => greaterEqual(lhs, rhs)
                case nme.LT  => less(lhs, rhs)
                case nme.LE  => lessEqual(lhs, rhs)
                case nme.ADD => intSum(lhs, rhs)
                case nme.SUB => intSum(lhs, intNegate(rhs))
                case nme.MUL => intProduct(lhs, rhs)
            case _ =>
              App(fromTree(fun), args.map(fromTree))
        case Select(qualifier, name) if tree.symbol.isRealMethod =>
          App(Ref(tree.symbol.termRef), List(fromTree(qualifier)))
        case Literal(c) if fromConst.isDefinedAt(c) =>
          fromConst(c)
        case _ if tree.tpe.isInstanceOf[SingletonType] =>
          Ref(tree.tpe.asInstanceOf[SingletonType])
        case _ =>
          throw new Error(f"Cannot translate ${tree}")

  val fromConst: PartialFunction[Constant, QualifierExpr] = {
    case Constant(value: Int)     => IntConst(value)
    case Constant(value: Double)  => DoubleConst(value)
    case Constant(value: String)  => StringConst(value)
    case Constant(value: Boolean) => if value then True else False
  }
