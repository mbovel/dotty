package dotty.tools.dotc
package qualifiers

import collection.mutable.ArrayBuffer
import java.nio.file.Files
import java.nio.file.Paths

object QualifierLogging:
  var enabled = false

  inline def log(inline msg: => String): Unit =
    config.Printers.qual.println(msg)

  enum LogEvent:
    case OfType(from: String, result: QualifierExpr)
    case FromTree(from: String, arg: String, result: QualifierExpr)
    case Push(currentFacts: String)
    case Assume(expr: QualifierExpr)
    case TypeTryImply(from: String, to: String, result: QualifierExpr)
    case Check(from: QualifierExpr, to: QualifierExpr, result: Boolean)
    case QualifierImplies(from: QualifierExpr, to: QualifierExpr, result: Boolean)
    case TryImply(from: QualifierExpr, to: QualifierExpr, frozen: Boolean, result: Boolean)
    case LeafImplies(from: QualifierExpr, to: QualifierExpr, result: Boolean)
    case LeafImpliesEquiv(from: QualifierExpr, to: QualifierExpr, result: Boolean)
    case TypeMap(from: QualifierExpr, result: QualifierExpr)
    case TryAddImplicationToLeaf(from: QualifierExpr, to: QualifierExpr, result: Boolean)
    case TryAddImplicationToVar(from: QualifierExpr, to: QualifierExpr, result: Boolean)

  import LogEvent.*

  private case class LogEventNode(event: LogEvent, children: ArrayBuffer[LogEventNode] = ArrayBuffer.empty)

  private var eventsStack = ArrayBuffer(ArrayBuffer[LogEventNode]())

  def trace(event: LogEvent): Unit =
    if !enabled then return
    eventsStack.last += LogEventNode(event)

  def trace[T](event: T => LogEvent)(compute: => T): T =
    if !enabled then return compute
    val children = ArrayBuffer[LogEventNode]()
    eventsStack += children
    val result = compute
    eventsStack.remove(eventsStack.length - 1)
    eventsStack.last += LogEventNode(event(result), children)
    result

  def startTrace(event: LogEvent): Unit =
    if !enabled then return
    val children = ArrayBuffer[LogEventNode]()
    eventsStack.last += LogEventNode(event, children)
    eventsStack += children

  def endTrace(): Unit =
    if !enabled then return
    eventsStack.remove(eventsStack.length - 1)

  private def eventNodeToJson(node: LogEventNode): String =
    val childrenJson = node.children.map(eventNodeToJson).mkString("[", ",", "]")
    s"""{"event": ${productToJson(node.event)}, "children": $childrenJson}"""

  private def productToJson(e: Product): String =
    val fields = e.productElementNames.zip(e.productIterator).map { (name, value) =>
      s""""$name": ${stringToJson(value.toString())}"""
    }.mkString(", ")
    val fields0 = if fields.isEmpty then "" else s", $fields"
    s"""{"$$type": "${e.productPrefix}"$fields0}"""

  // Escape special characters in a string
  private def stringToJson(s: String): String =
    val sb = new StringBuilder
    for c <- stripColors(s) do
      c match
        case '"' => sb ++= "\\\""
        case '\\' => sb ++= "\\\\"
        case '\b' => sb ++= "\\b"
        case '\f' => sb ++= "\\f"
        case '\n' => sb ++= "\\n"
        case '\r' => sb ++= "\\r"
        case '\t' => sb ++= "\\t"
        case c if c < ' ' => sb ++= f"\\u${c}%04x"
        case c => sb += c
    s"\"${sb.toString}\""

  def stripColors(line:String): String =
    // ESC has be seen in the wild replaced by "\u2190"
    // Also, BOM marker appears as ï»¿
    lazy val colorsRegex = "(\u001b|\u2190)\\[[0-9;]*m|ï»¿".r
    colorsRegex.replaceAllIn(line,"")

  case class NaiveSolverVars(exprs: IArray[QualifierExpr], dependencies: IArray[IArray[Int]])

  val naiveSolverVars = ArrayBuffer[NaiveSolverVars]()

  def logNaiveSolverVars(dependencies: IArray[IArray[QualifierExpr]]): Unit =
    if !enabled then return
    val exprs = IArray.from(dependencies.flatten.distinct)
    val vars = NaiveSolverVars(exprs, IArray.from(dependencies.map(_.map(exprs.indexOf(_)))))
    naiveSolverVars += vars

  var treeBefore: String | Null = null
  def logTreeBefore(tree: String): Unit =
    if !enabled then return
    this.treeBefore = tree

  var treeSetup: String | Null = null
  def logTreeSetup(tree: String): Unit =
    if !enabled then return
    this.treeSetup = tree

  var treeAfter: String | Null = null
  def logTreeAfter(tree: String): Unit =
    if !enabled then return
    this.treeAfter = tree

  def dumpLogs(file: String): Unit =
    assert(enabled)
    assert(eventsStack.length == 1)
    val eventsJson = eventsStack.head.map(eventNodeToJson).mkString("[", ",", "]")
    val naiveSolverVarsJson = naiveSolverVars.map { case NaiveSolverVars(exprs, dependencies) =>
      val exprsJson = exprs.map(_.toString).map(stringToJson).mkString("[", ",", "]")
      val dependenciesJson = dependencies.map { arr =>
        arr.map(_.toString).map(stringToJson).mkString("[", ",", "]")
      }.mkString("[", ",", "]")
      s"""{"exprs": $exprsJson, "dependencies": $dependenciesJson}"""
    }.mkString("[", ",", "]")
    val treeBeforeJson = if treeBefore == null then "null" else stringToJson(treeBefore.nn)
    val treeSetupJson = if treeSetup == null then "null" else stringToJson(treeSetup.nn)
    val treeAfterJson = if treeAfter == null then "null" else stringToJson(treeAfter.nn)
    val jsonStr: String = s"""
      {
        "trace": $eventsJson,
        "naiveSolverVars": $naiveSolverVarsJson,
        "treeBefore": $treeBeforeJson,
        "treeSetup": $treeSetupJson,
        "treeAfter": $treeAfterJson
      }""".stripIndent().nn
    Files.write(Paths.get(file), jsonStr.getBytes)

  def enableLogging(): Unit =
    eventsStack = ArrayBuffer(ArrayBuffer[LogEventNode]())
    naiveSolverVars.clear()
    treeBefore = null
    treeAfter = null
    enabled = true

  def disableLogging(): Unit =
    enabled = false
