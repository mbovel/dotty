package dotty.tools
package dotc
package qualifiers

import scala.collection.mutable

import core.*, Types.*, Symbols.*, Contexts.*, Names.*, ast.tpd.*
import StdNames.nme
import Constants.Constant
import Decorators.{i, toTermName}

import QualifierLogging.{trace, startTrace, endTrace, TraceEvent}
import QualifierExpr.*

object QualifierExprs:
  def fromType(tp: Type)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    trace[QualifierExpr](res => TraceEvent.OfType(tp.show, res.show)):
      tp.dealias match
        case QualifiedType(parent, pred) => and(pred, fromType(parent))
        case AndType(tp1, tp2)           => and(fromType(tp1), fromType(tp2))
        case OrType(tp1, tp2)            => if tp1.widen frozen_=:= tp2.widen then or(fromType(tp1), fromType(tp2)) else True
        case ConstantType(value)         => equal(PredArg, fromConst(value))
        case tp: ReferenceType           => equal(PredArg, Ref(tp)) // TODO(mbovel): fixme
        case tp: TypeProxy               => fromType(tp.underlying)
        case _                           => True

  val intBinOps: Set[Name] = Set(nme.EQ, nme.NE, nme.GT, nme.GE, nme.LT, nme.LE, nme.ADD, nme.MINUS, nme.MUL, nme.DIV)
  val boolBinsOps: Set[Name] = Set(nme.EQ, nme.NE, nme.ZAND, nme.ZOR)

  def fromClosure(tree: Tree)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    tree match
      case closureDef(meth) => fromTree(meth.rhs, meth.symbol, meth.paramss(0)(0).symbol)
      case _                => throw new Error(f"Cannot translate ${tree}")

  def fromTree(tree: Tree, lambdaSymbol: Symbol, argSymbol: Symbol)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    inline def rec(tree: Tree): QualifierExpr = fromTree(tree, lambdaSymbol, argSymbol)
    trace[QualifierExpr](res => TraceEvent.FromTree(tree.show, argSymbol.show, res.show)):
      tree match
        case id: Ident =>
          if id.symbol == argSymbol then PredArg
          else Ref(id.symbol.termRef)
        case Apply(fun, args) =>
          fun match
            case Select(qualifier, name) =>
              val lhs = rec(qualifier)
              val rhs = rec(args(0))
              val d = defn
              fun.symbol match
                case d.Boolean_== => equal(lhs, rhs)
                case d.Boolean_!= => notEqual(lhs, rhs)
                case d.Boolean_&& => and(lhs, rhs)
                case d.Boolean_|| => or(lhs, rhs)
                case d.Int_==     => equal(lhs, rhs)
                case d.Int_!=     => notEqual(lhs, rhs)
                case d.Int_>      => greater(lhs, rhs)
                case d.Int_>=     => greaterEqual(lhs, rhs)
                case d.Int_<      => less(lhs, rhs)
                case d.Int_<=     => lessEqual(lhs, rhs)
                case d.Int_+      => intSum(lhs, rhs)
                case d.Int_-      => intSum(lhs, intNegate(rhs))
                case d.Int_*      => intProduct(lhs, rhs)
                case _            => App(rec(fun), args.map(a => rec(a)))
            case _ => App(rec(fun), args.map(a => rec(a)))
        case Select(qualifier, name) =>
          val d = defn
          tree.symbol match
            case d.Boolean_!   => not(rec(qualifier))
            case d.Int_unary_- => intNegate(rec(qualifier))
            case _ =>
              rec(qualifier) match
                case Ref(tp) => Ref(tp.select(name.asTermName))
                case prefix  => Get(prefix, name)
        case Literal(c) if fromConst.isDefinedAt(c) =>
          fromConst(c)
        case _ =>
          tree.tpe match
            case tp: ReferenceType => Ref(tp)
            case _ =>
              report.error(i"This expression cannot be used in a type qualifier", tree.sourcePos)
              False

  def toClosure(expr: QualifierExpr, predArgType: Type)(using Context): Tree =
    val lambdaType: MethodType = MethodType(List("it".toTermName))(_ => List(predArgType), _ => defn.BooleanType)
    val res = Lambda(
      lambdaType,
      (args) =>
        val predArg = Ident(args(0).symbol.termRef)
        toTree(expr, predArg, predArgType)
    )
    // println(i"toClosure($expr, $predArgType) == $res")
    res

  def toTree(expr: QualifierExpr, predArg: Tree, predArgType: Type)(using Context): Tree =
    inline def rec(e: QualifierExpr): Tree = toTree(e, predArg, predArgType)

    def binOpToTree(name: Name, left: QualifierExpr, right: QualifierExpr, expectedType: Type): Tree =
      applyOverloaded(rec(left), name.toTermName, List(rec(right)), Nil, expectedType)

    def boolBinOpToTree(name: Name, left: QualifierExpr, right: QualifierExpr): Tree =
      binOpToTree(name, left, right, defn.BooleanType)

    def numberBinOpToTree(name: Name, left: QualifierExpr, right: QualifierExpr): Tree =
      binOpToTree(name, left, right, defn.IntType)

    def reduceWith(args: List[Tree], op: Symbol): Tree =
      args.reduce((a, b) => a.select(op).appliedTo(b))

    expr match
      case ApplyVar(i, arg)          => throw new Error("Cannot convert ApplyVar to Tree")
      case True                      => Literal(Constant(true))
      case False                     => Literal(Constant(false))
      case And(args)                 => args.map(rec(_)).reduce((a, b) => a.select(defn.Boolean_&&).appliedTo(b))
      case Or(args)                  => args.map(rec(_)).reduce((a, b) => a.select(defn.Boolean_||).appliedTo(b))
      case Not(arg)                  => rec(arg).select(defn.Boolean_!)
      case Equal(left, right)        => boolBinOpToTree(nme.EQ, left, right)
      case NotEqual(left, right)     => boolBinOpToTree(nme.NE, left, right)
      case Less(left, right)         => boolBinOpToTree(nme.LT, left, right)
      case LessEqual(left, right)    => boolBinOpToTree(nme.LE, left, right)
      case Greater(left, right)      => boolBinOpToTree(nme.GT, left, right)
      case GreaterEqual(left, right) => boolBinOpToTree(nme.GE, left, right)
      case PredArg                   => predArg
      case Ref(tp: TermParamRef)     => ast.untpd.Ident(tp.paramName).withType(tp)
      case Ref(tp: ReferenceType)    => ast.tpd.singleton(tp)
      case Get(prefix, name)         => rec(prefix).select(name)
      case App(fun, args)            => rec(fun).appliedToTermArgs(args.map(rec(_)))
      case IntSum(const, args)       => reduceWith(Literal(Constant(const)) :: args.map(rec(_)), defn.Int_+)
      case IntProduct(const, args)   => reduceWith(Literal(Constant(const)) :: args.map(rec(_)), defn.Int_*)
      case IntConst(value)           => Literal(Constant(value))
      case DoubleConst(value)        => Literal(Constant(value))
      case StringConst(value)        => Literal(Constant(value))

  val fromConst: PartialFunction[Constant, QualifierExpr] = {
    case Constant(value: Int)     => IntConst(value)
    case Constant(value: Double)  => DoubleConst(value)
    case Constant(value: String)  => StringConst(value)
    case Constant(value: Boolean) => if value then True else False
  }
