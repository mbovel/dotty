package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, Names.*, ast.tpd.*
import StdNames.nme
import util.Property.Key

import scala.collection.mutable
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.Text
import dotty.tools.dotc.qualifiers.QualifierLogging.log

import QualifierExpr.*
import dotty.tools.dotc.core.Decorators.{i, toTermName}

object QualifierExprs:
  private val cache = collection.mutable.HashMap[Type, QualifierExpr]()

  def ofType(tp: Type)(using Context): QualifierExpr =
    // TODO(mbovel): cache
    // cache.getOrElseUpdate(tp, computeofType(tp))
    val res = computeofType(tp)
    // println(f"ofType(${tp.show}) == ${res.show}")
    res

  private def computeofType(tp: Type)(using Context): QualifierExpr =
    import QualifierExpr.*
    tp.dealias match
      case QualifiedType(parent, pred)            => and(ofType(parent), pred)
      case AndType(tp1, tp2)                      => and(ofType(tp1), ofType(tp2))
      case OrType(tp1, tp2) if tp1 frozen_=:= tp2 => or(ofType(tp1), ofType(tp2))
      case tp: TypeProxy                          => ofType(tp.underlying)
      case _                                      => True

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
    // TODO(mbovel): cache
    val res = tree match
      case id: Ident =>
        if id.symbol == predArgSymbol then PredArg
        else fromSymbol(id.symbol)
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
      case Select(qualifier, name) if tree.symbol.isRealMethod =>
        App(fromSymbol(tree.symbol), List(fromTree(qualifier)))
      case Literal(c) if fromConst.isDefinedAt(c) =>
        fromConst(c)
      case _ =>
        throw new Error(f"Cannot translate ${tree}")
    log(i"fromTree($tree, $predArgSymbol) == ${res}")
    res


  def toClosure(expr: QualifierExpr, predArgType: Type)(using Context): Tree =
    val lambdaType: MethodType = MethodType(List("it".toTermName))(_ => List(predArgType), _ => defn.BooleanType)
    val res = ast.tpd.Lambda(lambdaType, (args) => {
      val predArg = Ident(args(0).symbol.termRef)
      toTree(expr, predArg, predArgType)
    })
    //println(i"toClosure($expr, $predArgType) == $res")
    res

  def toTree(expr: QualifierExpr, predArg: Tree, predArgType: Type)(using Context): Tree =
    expr match
      case ApplyVar(i, arg) => throw new Error("Cannot convert ApplyVar to Tree")
      case True => Literal(Constant(true))
      case False =>  Literal(Constant(false))
      case And(args) =>
        args.map(toTree(_, predArg, predArgType)).reduce((a, b) => Apply(Select(a, nme.AND), List(b)))
      case Or(args) =>
        args.map(toTree(_, predArg, predArgType)).reduce((a, b) => Apply(Select(a, nme.OR), List(b)))
      case Not(arg) =>
        Apply(Select(toTree(arg, predArg, predArgType), nme.UNARY_!), List())
      case Equal(left, right) =>
        val lhs = toTree(left, predArg, predArgType)
        val rhs = toTree(right, predArg, predArgType)
        println(i"lhs: ${lhs.tpe}, rhs: ${rhs.tpe}")
        Apply(Select(lhs, nme.EQ), List(rhs))

      case LessThan(left, right) =>
        val lhs = toTree(left, predArg, predArgType)
        val rhs = toTree(right, predArg, predArgType)
        Apply(Select(lhs, nme.LT), List(rhs))
      case PredArg => predArg
      case Ref(id, name) => EmptyTree
      case App(fun, args) => EmptyTree
      case QualifierExpr.Lambda(params, body) => EmptyTree
      case IntSum(const, args) =>
        val a = args.map(toTree(_, predArg, predArgType)).reduce((a, b) => Apply(Select(a, nme.ADD), List(b)))
        Apply(Select(a, nme.ADD), List(Literal(Constant(const))))
      case IntProduct(const, args) =>
        val a = args.map(toTree(_, predArg, predArgType)).reduce((a, b) => Apply(Select(a, nme.MUL), List(b)))
        Apply(Select(a, nme.MUL), List(Literal(Constant(const))))
      case IntConst(value) => Literal(Constant(value))
      case DoubleConst(value) => Literal(Constant(value))
      case StringConst(value) => Literal(Constant(value))

  val fromConst: PartialFunction[Constant, QualifierExpr] = {
    case Constant(value: Int)    => IntConst(value)
    case Constant(value: Double) => DoubleConst(value)
    case Constant(value: String) => StringConst(value)
    case Constant(value: Boolean) => if value then True else False
  }

  private val symToRef = collection.mutable.HashMap[Symbol, Ref]()
  def fromSymbol(sym: Symbol)(using Context): Ref =
    symToRef.getOrElseUpdate(sym, ctx.qualifierSolver.freshRef(sym.name.toString))
