package net.kogics.kojo.lite

import java.io.File

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.Position
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import com.sun.jdi.Bootstrap
import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame
import com.sun.jdi.ThreadReference
import com.sun.jdi.VirtualMachine
import com.sun.jdi.connect.Connector
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.EventSet
import com.sun.jdi.event.MethodEntryEvent
import com.sun.jdi.event.MethodExitEvent
import com.sun.jdi.event.VMDisconnectEvent
import com.sun.jdi.request.EventRequest

import java.awt.Paint
import java.awt.Color

import net.kogics.kojo.util.Utils
import net.kogics.kojo.core.CodeRunner

class Tracing(scriptEditor: ScriptEditor, builtins: Builtins) {
  var evtSet: EventSet = _
  var mainThread: ThreadReference = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()

  val reporter = new Reporter {
    override def info0(position: Position, msg: String, severity: Severity, force: Boolean) {
      severity.count += 1
      println(msg)
    }
  }

  val compiler = new Global(settings, reporter)
  val tracingGUI = new TracingGUI(scriptEditor)

  val wrapperCode = """
    import java.awt.Paint
    import java.awt.Color
    
    object Wrapper { 
    
  def main(args: Array[String]) { 
    %s
  }
  
   /* movement */
  def clear() {}
  def forward(n: Double) {}
  def right() {}
  def right(n: Double) {}
  def left() {}
  def left(n: Double) {}
  def back() {}
  def back(n: Double) {}
  def home() {}
  def jumpTo(x: Double, y: Double) {}
  def moveTo(x: Double, y: Double) {}
  def setPosition(x: Double, y: Double) {}
  def setPenColor(color: Paint) {}
  
  def repeat(n: Int) (fn: => Unit) {
    var i = 0
    while(i < n) {
      fn
      i += 1
    }
  }
} 
"""

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
    var optionValue = s"-classpath $tmpdir${File.pathSeparator}/tmp/scala-library.jar"

    if (mArgs == null)
      throw new Error("Bad launching connector");

    mArgs.setValue("Wrapper"); // assign args to main field
    opts.setValue(optionValue) //assign args to options field

    val vm = connector.launch(connArgs)
    vm
  }

  val ignoreMethods = Set("main", "<init>", "<clinit>", "$init$")
  val turtleMethods = Set("forward", "right", "clear", "back", "setPenColor")

  def trace(code: String) = Utils.runAsync {
    try {
      compile(code)
      //Connect to target VM
      val vm = launchVM()
      println("Attached to process '" + vm.name + "'")

      //Create Event Requests
      val excludes = Array("java.*", "javax.*", "sun.*", "com.sun.*", "com.apple.*")
      createRequests(excludes, vm);

      //Iterate through Events
      val evtQueue = vm.eventQueue
      vm.resume

      //Find main thread in target VM
      val allThrds = vm.allThreads
      allThrds.foreach { x => if (x.name == "main") mainThread = x }

      tracingGUI.reset

      breakable {
        while (true) {
          evtSet = evtQueue.remove()
          for (evt <- evtSet.eventIterator) {
            evt match {

              case methodEnterEvt: MethodEntryEvent =>
                if (!(ignoreMethods.contains(methodEnterEvt.method.name) || methodEnterEvt.method.name.startsWith("apply"))) {
                  try {
                    val frame = mainThread.frame(0)
                    val toprint =
                      if (methodEnterEvt.method().arguments().size > 0)
                        "(%s)" format methodEnterEvt.method.arguments.map { n =>
                          val argval = frame.getValue(n)

                          s"arg ${n.name}: ${n.`type`.name} = $argval"
                        }.mkString(",")
                      else ""

                    //determine if the method is a Turtle API method
                    if (turtleMethods contains methodEnterEvt.method.name) {
                      val desc = s"[Method Enter] ${methodEnterEvt.method.name}$toprint"
                      handleMethodEntry(
                        methodEnterEvt.method.name,
                        desc,
                        true,
                        mainThread.frame(0),
                        methodEnterEvt.method.arguments.toList,
                        mainThread.frame(1).location().lineNumber - 2,
                        mainThread.frame(1).location().sourceName)
                    }
                    else {
                      val desc = s"[Method Enter] ${methodEnterEvt.method.name}$toprint"
                      handleMethodEntry(
                        methodEnterEvt.method.name,
                        desc,
                        false,
                        mainThread.frame(0),
                        methodEnterEvt.method.arguments.toList,
                        methodEnterEvt.location.lineNumber - 2,
                        methodEnterEvt.location.sourceName)
                    }
                  }
                  catch {
                    case t: Throwable =>
                      println(s"[Exception] [Method Enter] ${methodEnterEvt.method.name} -- ${t.getMessage}")
                  }
                }

              case methodExitEvt: MethodExitEvent =>
                if (!(ignoreMethods.contains(methodExitEvt.method.name) || methodExitEvt.method.name.startsWith("apply"))) {
                  try {
                    //determine if the method is a Turtle API method
                    if (turtleMethods contains methodExitEvt.method.name) {
                      val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(
                        desc,
                        true,
                        mainThread.frame(0),
                        mainThread.frame(1).location.lineNumber - 2,
                        methodExitEvt.returnValue.toString,
                        mainThread.frame(1).location.sourceName)
                    }
                    else {
                      val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(
                        desc,
                        false,
                        mainThread.frame(0),
                        methodExitEvt.location.lineNumber - 2,
                        methodExitEvt.returnValue.toString,
                        methodExitEvt.location.sourceName
                      )
                    }
                  }
                  catch {
                    case t: Throwable =>
                      println(s"[Exception] [Method Exit] ${methodExitEvt.method.name} -- ${t.getMessage}")
                  }
                }
              case vmDcEvt: VMDisconnectEvent =>
                println("VM Disconnected"); break
              case _ => println("Other")
            }
          }
          evtSet.resume()
        }
      }
    }
    catch {
      case t: Throwable => System.err.println(s"[Exception] -- ${t.getMessage}")
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

  var currentMethodEvent: Option[MethodEvent] = None
  def handleMethodEntry(name: String, desc: String, isTurtle: Boolean, stkfrm: StackFrame, localArgs: List[LocalVariable], lineNum: Int, source: String) {
    var newEvt = new MethodEvent()
    newEvt.entry = desc
    newEvt.entryLineNum = lineNum
    newEvt.setEntryVars(stkfrm, localArgs)
    newEvt.setParent(currentMethodEvent)
    newEvt.sourceName = source
    currentMethodEvent = Some(newEvt)
    tracingGUI.addEvent(currentMethodEvent.get, source)
    if (isTurtle) {
      runTurtleMethod(name, stkfrm, localArgs)
    }
  }

  def runTurtleMethod(name: String, stkfrm: StackFrame, localArgs: List[LocalVariable]) {
    import builtins.Tw
    import builtins.TSCanvas
    name match {
        /*
      case "right" | "left"=>
        val method = Tw.getClass().getMethod(name)
        if (localArgs.length == 0) {
          method.invoke(Tw.getClass().newInstance())
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          method.invoke(Tw.getClass().newInstance(), angle: java.lang.Double)
        }
        */
      case "clear" =>
        TSCanvas.clear()
      case "forward" =>
        val step = stkfrm.getValue(localArgs(0)).toString.toDouble
        Tw.forward(step)
      case "right" =>
        if (localArgs.length == 0) {
          Tw.right()
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          Tw.right(angle)
        }
      case "back" =>
      	val step = stkfrm.getValue(localArgs(0)).toString.toDouble
        Tw.back(step)
      case "home" =>
        Tw.home
      case "jumpTo" => 
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        Tw.jumpTo(x, y)
      case "setPosition" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        Tw.setPosition(x, y)
      case "setPenColor" =>
        val name = stkfrm.getValue(localArgs(0)).toString
        val color: java.awt.Paint = Color.getColor(name)
        Tw.setPenColor(color)
      case _ =>
    }
  }

  def handleMethodExit(desc: String, isTurtle: Boolean, stkfrm: StackFrame, lineNum: Int, retVal: String, source: String) {
    currentMethodEvent.foreach { ce =>
      ce.isOver()
      ce.exit = desc
      ce.exitLineNum = lineNum
      ce.returnVal = retVal
      if (!isTurtle) {
        tracingGUI.addEvent(currentMethodEvent.get, source)
      }
      currentMethodEvent = ce.parent
    }
  }

  def createRequests(excludes: Array[String], vm: VirtualMachine) {
    val evtReqMgr = vm.eventRequestManager

    val mthdEnterVal = evtReqMgr.createMethodEntryRequest()
    excludes.foreach { mthdEnterVal.addClassExclusionFilter(_) }
    mthdEnterVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdEnterVal.enable()

    val mthdExitVal = evtReqMgr.createMethodExitRequest()
    excludes.foreach { mthdExitVal.addClassExclusionFilter(_) }
    mthdExitVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdExitVal.enable()
  }

}