package dotty.tools.dotc
package qualifiers

import collection.mutable.ArrayBuffer
import java.nio.file.Files
import java.nio.file.Paths

object QualifierLogging:
  var enabled = false

  inline def log(inline msg: => String): Unit =
    config.Printers.qual.println(msg)

  enum TraceEvent:
    case OfType(from: String, result: String)
    case FromTree(from: String, arg: String, result: String)
    case Push(currentFacts: String)
    case Assume(expr: String)
    case CheckExprConforms(from: String, to: String, result: String)
    case Check(from: String, to: String, result: Boolean)
    case QualifierImplies(from: String, to: String, result: Boolean)
    case TryImply(from: String, to: String, frozen: Boolean, result: Boolean)
    case LeafImplies(from: String, to: String, result: Boolean)
    case LeafImpliesEquiv(from: String, to: String, result: Boolean)
    case TypeMap(from: String, result: String)
    case TryAddImplicationToLeaf(from: String, to: String, result: Boolean)
    case TryAddImplicationToVar(from: String, to: String, result: Boolean)

  import TraceEvent.*

  private case class TraceEventNode(event: TraceEvent, children: ArrayBuffer[TraceEventNode] = ArrayBuffer.empty)

  private var eventsStack = ArrayBuffer(ArrayBuffer[TraceEventNode]())

  def trace(event: TraceEvent): Unit =
    if !enabled then return
    eventsStack.last += TraceEventNode(event)

  def trace[T](event: T => TraceEvent)(compute: => T): T =
    if !enabled then return compute
    val children = ArrayBuffer[TraceEventNode]()
    eventsStack += children
    val result = compute
    eventsStack.remove(eventsStack.length - 1)
    eventsStack.last += TraceEventNode(event(result), children)
    result

  def startTrace(event: TraceEvent): Unit =
    if !enabled then return
    val children = ArrayBuffer[TraceEventNode]()
    eventsStack.last += TraceEventNode(event, children)
    eventsStack += children

  def endTrace(): Unit =
    if !enabled then return
    eventsStack.remove(eventsStack.length - 1)

  private def eventNodeToJson(node: TraceEventNode): String =
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

  case class NaiveSolverVars(exprs: IArray[String], dependencies: IArray[IArray[Int]])

  val naiveSolverVars = ArrayBuffer[NaiveSolverVars]()

  def logNaiveSolverVars(dependencies: IArray[IArray[String]]): Unit =
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
    eventsStack = ArrayBuffer(ArrayBuffer[TraceEventNode]())
    naiveSolverVars.clear()
    treeBefore = null
    treeAfter = null
    enabled = true

  def disableLogging(): Unit =
    enabled = false
