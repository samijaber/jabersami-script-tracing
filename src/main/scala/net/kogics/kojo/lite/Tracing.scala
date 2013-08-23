/*
 * Copyright (C) 2013 "Sami Jaber" <jabersami@gmail.com>
 * Copyright (C) 2013 Lalit Pant <pant.lalit@gmail.com>
 *
 * The contents of this file are subject to the GNU General Public License
 * Version 3 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.gnu.org/copyleft/gpl.html
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 */
package net.kogics.kojo.lite

import java.awt.Color
import java.awt.geom.Point2D
import java.io.File
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.mutable.HashMap
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.Position
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
import scala.util.matching.Regex
import com.sun.jdi.AbsentInformationException
import com.sun.jdi.ArrayReference
import com.sun.jdi.Bootstrap
import com.sun.jdi.IntegerValue
import com.sun.jdi.InvocationException
import com.sun.jdi.LocalVariable
import com.sun.jdi.ObjectReference
import com.sun.jdi.StackFrame
import com.sun.jdi.StringReference
import com.sun.jdi.ThreadReference
import com.sun.jdi.Value
import com.sun.jdi.VirtualMachine
import com.sun.jdi.connect.Connector
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.EventSet
import com.sun.jdi.event.MethodEntryEvent
import com.sun.jdi.event.MethodExitEvent
import com.sun.jdi.event.ThreadStartEvent
import com.sun.jdi.event.VMDeathEvent
import com.sun.jdi.event.VMDisconnectEvent
import com.sun.jdi.event.VMStartEvent
import com.sun.jdi.request.EventRequest
import net.kogics.kojo.core.Turtle
import net.kogics.kojo.util.Utils
import javax.swing.JFrame
import javax.swing.JScrollPane
import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import com.sun.jdi.BooleanValue
import com.sun.jdi.PrimitiveValue
import com.sun.jdi.Field
import scala.collection.parallel.ThrowableOps
import com.sun.jdi.ClassObjectReference

class Tracing(scriptEditor: ScriptEditor, builtins: Builtins) {
  var evtSet: EventSet = _
  var currThread: ThreadReference = _
  var mainThread: ThreadReference = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()
  val turtles = new HashMap[Long, Turtle]
  var evtReqs = Vector[EventRequest]()

  var currEvtVec = Vector[(String, Option[MethodEvent])](("main", None))
  var CurrMthdEvtIndex: Int = -1

  val reporter = new Reporter {
    override def info0(position: Position, msg: String, severity: Severity, force: Boolean) {
      severity.count += 1
      println(msg)
    }
  }
  val compiler = new Global(settings, reporter)
  val tracingGUI = new TracingGUI(scriptEditor, builtins.kojoCtx)
  val lineNumOffset = 3

  val wrapperCode = """object Wrapper {
import net.kogics.kojo.lite.TracingBuiltins._
def main(args: Array[String]) { 
    %s
  }
} 
"""

  def stop() {
    currThread.virtualMachine().exit(1)
  }

  def compile(code0: String) = {
    val code = wrapperCode format code0
    val codeFile = new BatchSourceFile("scripteditor", code)
    val run = new compiler.Run
    reporter.reset
    run.compileSources(List(codeFile))
    if (reporter.hasErrors) {
      throw new RuntimeException("Trace Compilation Error. Ensure that your program compiles correctly before trying to trace it.")
    }
  }

  def makeSettings() = {
    val iSettings = new Settings()
    iSettings.usejavacp.value = true
    iSettings.outputDirs.setSingleOutput(tmpdir)
    iSettings
  }

  def launchVM() = {
    var connector: LaunchingConnector = null
    val conns = Bootstrap.virtualMachineManager().allConnectors();
    breakable {
      for (conn <- conns) {
        if (conn.name().equals("com.sun.jdi.CommandLineLaunch")) {
          connector = conn.asInstanceOf[LaunchingConnector]
          break
        }
      }
    }

    // set connector arguments
    val connArgs = connector.defaultArguments();
    val mArgs = connArgs.get("main").asInstanceOf[Connector.Argument]
    val opts = connArgs.get("options").asInstanceOf[Connector.Argument]
    var optionValue = s"""-classpath "$tmpdir${File.pathSeparator}${System.getProperty("java.class.path")}" """

    if (mArgs == null)
      throw new Error("Bad launching connector");

    mArgs.setValue("Wrapper"); // assign args to main field
    opts.setValue(optionValue) //assign args to options field

    val vm = connector.launch(connArgs)
    vm
  }

  val ignoreMethods = Set("main", "<init>", "<clinit>", "$init$", "repeat", "repeatWhile", "runInBackground", "inspect")
  val turtleMethods = Set("setBackground", "color", "forward", "right", "left", "turn", "clear", "cleari", "invisible", "jumpTo", "back", "setPenColor", "setFillColor", "setAnimationDelay", "setPenThickness", "penDown", "penUp", "circle", "savePosHe", "restorePosHe", "newTurtle", "changePosition", "scaleCostume", "setCostumes", "axesOn", "axesOff", "gridOn", "gridOff", "zoom", "inspectx")

  def getThread(vm: VirtualMachine, name: String): ThreadReference = {
    try {
      //Find main thread in target VM
      val allThrds = vm.allThreads
      var index: Int = allThrds.indexWhere(x => x.name == name)
      allThrds(index)
    }
    catch {
      case e: ArrayIndexOutOfBoundsException =>
        println("a Thread with name " + name + "does not exist")
        return null
    }
  }

  val primitives: Set[Class[_]] = Set(
    java.lang.Boolean.TYPE,
    java.lang.Character.TYPE,
    java.lang.Byte.TYPE,
    java.lang.Short.TYPE,
    java.lang.Integer.TYPE,
    java.lang.Long.TYPE,
    java.lang.Float.TYPE,
    java.lang.Double.TYPE
  )

  val ignoreNodes = Vector("Static Fields", "Inherited Static Fields", "Inherited Fields")

  //def sortFields(f1: Field, f2: Field) = simplifyStr(f1.toString) < simplifyStr(f2.toString)
  def simplifyStr(name: String): String = {
    name
    //name.splitAt(name.lastIndexOf(".") + 1)._2
  }
  def addChildren[T](obj: T, node: DefaultMutableTreeNode) {
    val staticFields = new DefaultMutableTreeNode("Static Fields")
    val inStaticFields = new DefaultMutableTreeNode("Inherited Static Fields")
    val inFields = new DefaultMutableTreeNode("Inherited Fields")
    val nodeArr = Vector(staticFields, inStaticFields, inFields)

    var fields = obj.getClass().getDeclaredFields() //.sortWith(sortFields)
    fields.foreach { field =>
      field.setAccessible(true)
      field.get(obj) match {
        case null =>
        case arr: Array[Any] =>
          val elementData = new DefaultMutableTreeNode(simplifyStr(field.toString) + ": " + field.getType().getName() + "=" + "(%s)" format arr.map { n => s"$n" }.mkString(","))
          node add elementData
          for (index <- 0 to arr.size - 1) {
            val idxNode = new DefaultMutableTreeNode(index)
            elementData add idxNode
            idxNode add (if (arr(index) == null) new DefaultMutableTreeNode("null", false) else new DefaultMutableTreeNode(arr(index)))
          }
        case fieldVal =>
          val fieldNode = if (!primitives.contains(field.getType))
            new DefaultMutableTreeNode(fieldVal, true)
          else
            new DefaultMutableTreeNode(fieldVal, false)

          node add fieldNode
        /*
          if (Modifier.isStatic(field.getModifiers)) {
            staticFields add fieldNode
            fieldNode setParent staticFields
          }
          else {
            node add fieldNode
            fieldNode setParent node
          }*/
      }
    }

    var superClass = obj.getClass().getSuperclass
    while (superClass != null) {
      val fields = superClass.getDeclaredFields() //.sortWith(sortFields)
      fields.foreach { field =>
        field.setAccessible(true);
        val fieldNode = if (field.get(obj) != null && !primitives.contains(field.getType))
          new DefaultMutableTreeNode(field.get(obj), true)
        else
          new DefaultMutableTreeNode(field.get(obj), false)

        node add fieldNode
        /*
        if (Modifier.isStatic(field.getModifiers)) {
          inStaticFields add fieldNode
          fieldNode setParent inStaticFields
        }
        else {
          inFields add fieldNode
          fieldNode setParent inFields
        }*/
      }
      superClass = superClass.getSuperclass
    }
    nodeArr.foreach(n => if (n.getChildCount > 0) node add n)
  }

  def addChildren(obj: ObjectReference, node: DefaultMutableTreeNode) {
    val staticFields = new DefaultMutableTreeNode("Static Fields")
    val inStaticFields = new DefaultMutableTreeNode("Inherited Static Fields")
    val inFields = new DefaultMutableTreeNode("Inherited Fields")
    val nodeArr = Vector(staticFields, inStaticFields, inFields)

    var fields = obj.referenceType().allFields()
    fields.foreach { field =>
      var fieldVal = obj.getValue(field)
      println("value for field " + field + " is: " + fieldVal)

      fieldVal.`type` match {
        case null =>
          /*
        case Array[Any] =>
          val arr = fieldVal.asInstanceOf[ArrayReference].getValues
          val elementData = new DefaultMutableTreeNode(simplifyStr(field.toString) + ": " + field.`type`.name + "=" + "(%s)" format arr.map { n => s"$n" }.mkString(","))
          node add elementData
          for (index <- 0 to arr.size - 1) {
            val idxNode = new DefaultMutableTreeNode(index)
            elementData add idxNode
            idxNode add (if (arr(index) == null) new DefaultMutableTreeNode("null", false) else new DefaultMutableTreeNode(arr(index)))
          }*/
        case _ =>
          val fieldNode = if (field.isInstanceOf[PrimitiveValue])
            new DefaultMutableTreeNode(fieldVal, true)
          else
            new DefaultMutableTreeNode(fieldVal, false)
          node add fieldNode
        /*
          if (Modifier.isStatic(field.getModifiers)) {
            staticFields add fieldNode
            fieldNode setParent staticFields
          }
          else {
            node add fieldNode
            fieldNode setParent node
          }*/
      }
    }
    /*
    var superClass = obj.getClass().getSuperclass
    while (superClass != null) {
      val fields = superClass.getDeclaredFields() //.sortWith(sortFields)
      fields.foreach { field =>
        field.setAccessible(true);
        val fieldNode = if (field.get(obj) != null && !primitives.contains(field.getType))
          new DefaultMutableTreeNode(field.get(obj), true)
        else
          new DefaultMutableTreeNode(field.get(obj), false)

        node add fieldNode
        /*
        if (Modifier.isStatic(field.getModifiers)) {
          inStaticFields add fieldNode
          fieldNode setParent inStaticFields
        }
        else {
          inFields add fieldNode
          fieldNode setParent inFields
        }*/
      }
      superClass = superClass.getSuperclass
    }*/
    //nodeArr.foreach(n => if (n.getChildCount > 0) node add n)
  }

  def inspect(obj: Value, name: String) {
    val panel = new JFrame("Object Inspection") {
      setVisible(true)
      val root = new DefaultMutableTreeNode(obj)
      addChildren(obj.asInstanceOf[ObjectReference], root)
      var tree = new JTree(root) {
        addMouseListener(new MouseAdapter {
          override def mouseClicked(e: MouseEvent) {
            val node = getLastSelectedPathComponent
            val nodeContent = node.asInstanceOf[DefaultMutableTreeNode]
            val objt = nodeContent.getUserObject
            if (nodeContent.getAllowsChildren() && nodeContent.getChildCount() == 0 && !ignoreNodes.contains(objt))
              addChildren(objt, nodeContent)
          }
        })
      }
      var view = new JScrollPane(tree)
      getContentPane add view
    }
  }

  def trace(code: String) = Utils.runAsync {
    try {
      turtles.clear()
      currEvtVec = Vector[(String, Option[MethodEvent])](("main", None))

      compile(code)
      //Connect to target VM
      val vm = launchVM()
      println("Attached to process '" + vm.name + "'")

      //Create Event Requests
      val excludes = Array("java.*", "javax.*", "sun.*", "com.sun.*", "com.apple.*", "edu.umd.cs.piccolo.*")
      mainThread = getThread(vm, "main")
      currThread = getThread(vm, "main")
      createRequests(excludes, vm, mainThread);
      WatchThreadStarts

      //Iterate through Events
      val evtQueue = vm.eventQueue
      vm.resume

      tracingGUI.reset

      breakable {
        while (true) {
          evtSet = evtQueue.remove()
          for (evt <- evtSet.eventIterator) {
            evt match {
              case threadStartEvt: ThreadStartEvent =>
                var name = threadStartEvt.thread().name
                if (name.contains("Thread-")) {
                  createRequests(excludes, vm, threadStartEvt.thread())
                  incrementCurrEvt(name)
                }
              case methodEnterEvt: MethodEntryEvent =>
                // println("Method entered: " + methodEnterEvt.method().name())
                /*
                if (methodEnterEvt.method.name == "main") {
                  println("main method was exited. Events left are:")
                  evtReqs.foreach(evt =>
                    println("event is enabled? " + evt.isEnabled + ". and is " + evt)
                  )
                }
                * 
                */
                evtReqs.foreach(x => x.disable)
                currThread = methodEnterEvt.thread()
                if (!(ignoreMethods.contains(methodEnterEvt.method.name) || methodEnterEvt.method.name.startsWith("apply"))) {
                  try {
                    val toprint = try {
                      if (false) //methodEnterEvt.method.arguments.size > 0)
                        "(%s)" format methodEnterEvt.method.arguments.map { n =>
                          val frame = methodEnterEvt.thread.frame(0)
                          val frameVal = frame.getValue(n)

                          val argval = if (frameVal.isInstanceOf[ObjectReference] &&
                            !frameVal.isInstanceOf[StringReference] &&
                            !frameVal.isInstanceOf[ArrayReference]) {
                            //println("getting argval")
                            val objRef = frameVal.asInstanceOf[ObjectReference]
                            val mthd = objRef.referenceType.methodsByName("toString")(0)

                            val rtrndValue = try {
                              var objVal = objRef.invokeMethod(currThread, mthd, new java.util.ArrayList, 0)
                              objVal.asInstanceOf[StringReference].value()
                            }
                            catch {
                              case inv: InvocationException =>
                                println("[Exception] error in invokeMethod/target VM")
                                //inv.printStackTrace()
                                frameVal
                              case illArg: IllegalArgumentException =>
                                println("[Exception] invokeMethod to toString expected arguments")
                                frameVal
                              case _: Throwable =>
                                println("[Exception] other invokeMethod error")
                                frameVal
                            }
                          }
                          else {
                            frameVal
                          }
                          // println("Argval done")
                          s"arg ${n.name}: ${n.typeName} = $argval"
                        }.mkString(",")
                      else "()"
                    }
                    catch {
                      case e: AbsentInformationException => "There is an AbsentInformationException"
                    }

                    val desc = s"[Method Enter] ${methodEnterEvt.method.name}$toprint"

                    val argList = try { methodEnterEvt.method.arguments.toList }
                    catch { case e: AbsentInformationException => List[LocalVariable]() }

                    val callerSrcName = try { currThread.frame(1).location.sourceName }
                    catch { case _: Throwable => "N/A" }

                    val callerLineNum = try { currThread.frame(1).location().lineNumber - lineNumOffset }
                    catch { case _: Throwable => -1 }

                    val callerLine = try { scriptEditor.getTraceLine(callerLineNum) }
                    catch { case _: Throwable => "N/A" }

                    val srcName = try { methodEnterEvt.location().sourceName }
                    catch { case e: Throwable => "" }

                    handleMethodEntry(
                      methodEnterEvt.method.name,
                      desc,
                      turtleMethods contains methodEnterEvt.method.name,
                      currThread.frame(0),
                      argList,
                      methodEnterEvt.location.lineNumber - lineNumOffset,
                      srcName,
                      callerSrcName,
                      callerLine,
                      callerLineNum
                    )
                    //println("Method entered done: " + methodEnterEvt.method().name())
                  }
                  catch {
                    case t: Throwable =>
                      println(t.printStackTrace())
                    //println(s"[Exception] [Method Enter] [${t.getClass()}] ${methodEnterEvt.method.name} -- ${t.getMessage}")
                  }
                }
                evtReqs.foreach(x => x.enable)
              case methodExitEvt: MethodExitEvent =>
                // println("Method exit: " + methodExitEvt.method.name)
                /* if (methodExitEvt.method.name == "main") {
                  println("main method was exited. Events left are:")
                  evtReqs.foreach(evt =>
                    println("event is enabled? " + evt.isEnabled + ". and is " + evt)
                  )
                }*/
                if (!(ignoreMethods.contains(methodExitEvt.method.name) || methodExitEvt.method.name.startsWith("apply"))) {
                  try {
                    currThread = methodExitEvt.thread()
                    val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                    val argList = try { methodExitEvt.method.arguments.toList }
                    catch { case e: AbsentInformationException => List[LocalVariable]() }

                    handleMethodExit(
                      methodExitEvt.method.name,
                      desc,
                      turtleMethods contains methodExitEvt.method.name,
                      currThread.frame(0),
                      argList,
                      methodExitEvt.location.lineNumber - lineNumOffset,
                      methodExitEvt.returnValue
                    )
                    // println("Method exit evt done: " + methodExitEvt.method().name())
                  }
                  catch {
                    case t: Throwable =>
                      println(t.printStackTrace())
                    //println(s"[Exception] [Method Exit] [${t.getClass()}] ${methodExitEvt.method.name} -- ${t.getMessage}")
                  }
                }
              case vmDcEvt: VMDisconnectEvent =>
                println("VM Disconnected"); break
              case vmStartEvt: VMStartEvent =>
                println("VM Started")
              case vmDeathEvt: VMDeathEvent =>
                println("VM Dead")
              case _ =>
                println("Other")
            }
          }
          evtSet.resume()
        }
      }
    }
    catch {
      case t: Throwable => System.err.println(s"[Exception] -- ${t.getMessage}")
      //t.printStackTrace()
    }
  }

  def printFrameVarInfo(stkfrm: StackFrame) {
    try {
      println(s"Visible Vars: ${stkfrm.visibleVariables}")
      println(s"Argument Values: ${stkfrm.getArgumentValues}")
    }
    catch {
      case t: Throwable =>
    }
  }

  def getCurrMthdEvtIndex: Int = try {
    var name = currThread.name
    currEvtVec.indexWhere(evt => evt._1 == name)
  }
  catch {
    case t: Throwable => println("could not find MethodEventIndex for thread " + currThread.name); -1
  }

  def getCurrentMethodEvent: Option[MethodEvent] = try {
    var name = currThread.name
    var index = currEvtVec.indexWhere(evt => evt._1 == name)
    currEvtVec(index)._2
  }
  catch {
    case t: Throwable =>
      println("could not find MethodEvent for thread " + currThread.name + ". Created one")
      incrementCurrEvt(currThread.name)
      currEvtVec.last._2
  }

  def updateMethodEventVector(newEvt: Option[MethodEvent]) {
    var index = currEvtVec.indexWhere(evt => evt._1 == currThread.name)
    currEvtVec = currEvtVec.updated(index, (currThread.name, newEvt))
  }

  def incrementCurrEvt(name: String) {
    if (currEvtVec.indexWhere(x => x._1 == name) == -1)
      currEvtVec = currEvtVec :+ (name, None)
  }

  def handleMethodEntry(name: String, desc: String, isTurtle: Boolean, stkfrm: StackFrame, localArgs: List[LocalVariable], lineNum: Int, source: String, callerSource: String, callerLine: String, callerLineNum: Int) {
    var newEvt = new MethodEvent()
    var mthdEvent = getCurrentMethodEvent
    newEvt.entry = desc
    newEvt.entryLineNum = lineNum
    newEvt.setEntryVars(stkfrm, localArgs)
    newEvt.setParent(mthdEvent)
    newEvt.sourceName = source
    newEvt.callerSourceName = callerSource
    newEvt.callerLine = callerLine
    newEvt.callerLineNum = callerLineNum
    newEvt.methodName = name

    updateMethodEventVector(Some(newEvt))

    var ret: Option[(Point2D.Double, Point2D.Double)] = None
    if (isTurtle) {
      ret = runTurtleMethod(name, stkfrm, localArgs)
    }
    tracingGUI.addEvent(newEvt, ret)
  }

  def handleMethodExit(name: String, desc: String, isTurtle: Boolean, stkfrm: StackFrame, localArgs: List[LocalVariable], lineNum: Int, retVal: Value) {
    def retValStr: String = { if (retVal == null) "" else retVal.toString }
    runTurtleMethod2(name, stkfrm, localArgs, retVal)

    var mthdEvent = getCurrentMethodEvent
    mthdEvent.foreach { ce =>
      ce.isOver()
      ce.exit = desc
      ce.exitLineNum = lineNum
      ce.returnVal = retValStr

      tracingGUI.addEvent(ce, None)

      mthdEvent = ce.parent
    }
    updateMethodEventVector(mthdEvent)
  }

  def runTurtleMethod(name: String, stkfrm: StackFrame, localArgs: List[LocalVariable]): Option[(Point2D.Double, Point2D.Double)] = {
    if (stkfrm.thisObject() == null) return None

    import builtins.Tw
    import builtins.TSCanvas
    var ret: Option[(Point2D.Double, Point2D.Double)] = None

    val turtle = {
      val caller = stkfrm.thisObject().uniqueID()
      //    if (turtles.contains(caller)) {
      //      println(s"$name call for turtle with id: $caller")
      //    }
      //    else {
      //      println(s"$name call for default turtle")
      //    }
      turtles.getOrElse(caller, Tw.getTurtle)
    }

    name match {
      case "inspectx" =>
        val obj = stkfrm.getValue(localArgs(0))
        val x = stkfrm.getValue(localArgs(1)).toString
        inspect(obj, x)
      case "clear" =>
        TSCanvas.clear()
      case "cleari" =>
        TSCanvas.cleari()
      case "invisible" =>
        turtle.invisible
      case "forward" =>
        if (localArgs.length == 1) {
          val step = stkfrm.getValue(localArgs(0)).toString.toDouble
          turtle.forward(step)
          ret = Some(turtle.lastLine)
        }
      case "turn" =>
        val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.turn(angle)
      case "right" =>
      case "left"  =>
      case "back"  =>
      case "home" =>
        turtle.home
      case "jumpTo" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.jumpTo(x, y)
      case "setCostume" =>
        val str = stkfrm.getValue(localArgs(0)).toString
        turtle.setCostume(str)
      case "setPosition" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.setPosition(x, y)
      case "setPenColor" =>
        val color = getColor(stkfrm, localArgs)
        turtle.setPenColor(color)
      case "setFillColor" =>
        val color = getColor(stkfrm, localArgs)
        turtle.setFillColor(color)
      case "setAnimationDelay" =>
        val step = stkfrm.getValue(localArgs(0)).toString.toLong
        turtle.setAnimationDelay(step)
      case "setPenThickness" =>
        val thickness = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.setPenThickness(thickness)
      case "penUp" =>
        turtle.penUp
      case "penDown" =>
        turtle.penDown
      case "circle" =>
        val r = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.arc(r, 360)
      case "savePosHe" =>
        turtle.savePosHe
      case "restorePosHe" =>
        turtle.restorePosHe
      case "newTurtle" =>
      // handled on the exit event
      case "changePosition" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.changePosition(x, y)
      case "scaleCostume" =>
        val a = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.scaleCostume(a)
      case "setCostumes" =>
        val (a, b) = (stkfrm.getValue(localArgs(0)).toString, stkfrm.getValue(localArgs(1)).toString)
        turtle.setCostumes(a, b)
      case "setBackground" =>
        var c = getColor(stkfrm, localArgs)
        TSCanvas.tCanvas.setCanvasBackground(c)
      case "axesOn" =>
        TSCanvas.axesOn
      case "axesOff" =>
        TSCanvas.axesOff
      case "gridOn" =>
        TSCanvas.gridOn
      case "gridOff" =>
        TSCanvas.gridOff
      case "zoom" =>
        val (x, y, z) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble, stkfrm.getValue(localArgs(0)).toString.toDouble)
        TSCanvas.zoom(x, y, z)
      case _ =>
    }
    ret
  }

  def runTurtleMethod2(name: String, stkfrm: StackFrame, localArgs: List[LocalVariable], retVal: Value) {
    name match {
      case "newTurtle" =>
        import builtins.TSCanvas
        if (localArgs.length == 3) {
          val (x, y, str) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble, stkfrm.getValue(localArgs(2)).toString)
          val newTurtle = TSCanvas.newTurtle(x, y, str.slice(1, str.length - 1))
          val ref = retVal.asInstanceOf[ObjectReference].uniqueID()
          //          println(s"New turtle $ref mapped")
          turtles(ref) = newTurtle
        }

      case _ =>
    }
  }

  def getColor(stkfrm: StackFrame, localArgs: List[LocalVariable]): Color = {
    var colorVal = stkfrm.getValue(localArgs(0)).asInstanceOf[ObjectReference]

    var mthd = colorVal.referenceType.methodsByName("toString")(0)
    var rtrndValue = colorVal.invokeMethod(currThread, mthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
    var str = rtrndValue.asInstanceOf[StringReference].value()
    var pattern = new Regex("\\d{1,3}")
    var rgb = Vector[Int]()
    (pattern findAllIn str).foreach(c => rgb = rgb :+ c.toInt)

    var alphaMthd = colorVal.referenceType.methodsByName("getAlpha")(0)
    var alphaValue = colorVal.invokeMethod(currThread, alphaMthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
    var alpha = alphaValue.asInstanceOf[IntegerValue].value

    new Color(rgb(0), rgb(1), rgb(2), alpha)
  }

  def WatchThreadStarts() {
    val evtReqMgr = currThread.virtualMachine().eventRequestManager

    val thrdStartVal = evtReqMgr.createThreadStartRequest()
    thrdStartVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    //thrdStartVal.addThreadFilter(mainThread)
    thrdStartVal.enable()
    evtReqs = evtReqs :+ thrdStartVal
  }

  def createRequests(excludes: Array[String], vm: VirtualMachine, thread: ThreadReference) {
    val evtReqMgr = vm.eventRequestManager

    val mthdEnterVal = evtReqMgr.createMethodEntryRequest()
    excludes.foreach { mthdEnterVal.addClassExclusionFilter(_) }
    mthdEnterVal.addThreadFilter(thread)
    mthdEnterVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdEnterVal.enable()
    evtReqs = evtReqs :+ mthdEnterVal

    val mthdExitVal = evtReqMgr.createMethodExitRequest()
    excludes.foreach { mthdExitVal.addClassExclusionFilter(_) }
    mthdExitVal.addThreadFilter(thread)
    mthdExitVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdExitVal.enable()
    evtReqs = evtReqs :+ mthdExitVal
  }

}