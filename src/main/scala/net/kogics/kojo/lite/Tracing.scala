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
import com.sun.jdi.event.ThreadStartEvent
import com.sun.jdi.request.EventRequest
import com.sun.jdi.ClassType
import com.sun.jdi.ObjectReference
import com.sun.jdi.StringReference
import com.sun.jdi.AbsentInformationException
import java.awt.Paint
import java.awt.Color
import scala.util.matching.Regex
import net.kogics.kojo.util.Utils
import net.kogics.kojo.core.Turtle

class Tracing(scriptEditor: ScriptEditor, builtins: Builtins) {
  var evtSet: EventSet = _
  var mainThread: ThreadReference = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()
  var turtles = Vector[Turtle]()
  var turtlesRefs = Vector[Long]()
  var runningInBG = false
  
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

  def stop(){
    mainThread.virtualMachine().exit(0)
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

  val ignoreMethods = Set("main", "<init>", "<clinit>", "$init$", "repeat", "runInBackground")
  val turtleMethods = Set("forward", "right", "left", "clear", "cleari", "invisible", "back", "setPenColor", "setFillColor", "setAnimationDelay", "setPenThickness", "penDown", "penUp", "circle", "newTurtle")
  
  def getThread(vm: VirtualMachine, name: String) {
      //Find main thread in target VM
      val allThrds = vm.allThreads
      allThrds.foreach { x => if (x.name == name) mainThread = x }
  }
  
  
  def trace(code: String) = Utils.runAsync {
    
    try {
      turtles = Vector[Turtle]()
      turtlesRefs = Vector[Long]()
      
      compile(code)
      //Connect to target VM
      val vm = launchVM()
      println("Attached to process '" + vm.name + "'")

      //Create Event Requests
      val excludes = Array("java.*", "javax.*", "sun.*", "com.sun.*", "com.apple.*")
      getThread(vm, "main")
      createRequests(excludes, vm);
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
        	      println("@#@#@#@#@: " + name)
        	      if (name.contains("Thread-"))
        	        addThreadReqs(threadStartEvt.thread())
              case methodEnterEvt: MethodEntryEvent =>
                mainThread = methodEnterEvt.thread()
                
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
                      //if(mainThread.frame(1).location().sourceName != "scripteditor") {break;}
                      val desc = s"[Method Enter] ${methodEnterEvt.method.name}$toprint"
                      handleMethodEntry(
                        methodEnterEvt.method.name,
                        desc,
                        true,
                        mainThread.frame(0),
                        methodEnterEvt.method.arguments.toList,
                        mainThread.frame(1).location().lineNumber - lineNumOffset,
                        mainThread.frame(1).location().sourceName)
                    }
                    else {
                      //if(methodEnterEvt.location.sourceName != "scripteditor") {break;}
                      val desc = s"[Method Enter] ${methodEnterEvt.method.name}$toprint"
                      handleMethodEntry(
                        methodEnterEvt.method.name,
                        desc,
                        false,
                        mainThread.frame(0),
                        methodEnterEvt.method.arguments.toList,
                        methodEnterEvt.location.lineNumber - lineNumOffset,
                        methodEnterEvt.location.sourceName)
                    }
                  }
                  catch {
                    case abs: AbsentInformationException => 
                    case t: Throwable =>
                      println(s"${methodEnterEvt.thread().name()} [Exception] [Method Enter] ${methodEnterEvt.method.name} -- ${t.getMessage}")
                  }
                }

              case methodExitEvt: MethodExitEvent =>
                if (!(ignoreMethods.contains(methodExitEvt.method.name) || methodExitEvt.method.name.startsWith("apply"))) {
                  try {
                	mainThread = methodExitEvt.thread()
                    
                    //Vectors to keep track of new turtlesRefs
                    if (methodExitEvt.method.name == "newTurtle" && isFromWrapper(mainThread.frame(0)))
                    {
                      var ref = methodExitEvt.returnValue().asInstanceOf[ObjectReference].uniqueID()
                      //println("@$*#$#$#$#$#$# Adding ID: " + ref)
                      turtlesRefs = turtlesRefs :+ ref
                    }
                    
                    //determine if the method is a Turtle API method
                    if (turtleMethods contains methodExitEvt.method.name) {
                      //if(mainThread.frame(1).location().sourceName != "scripteditor"){break;}

                      val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(
                        desc,
                        true,
                        mainThread.frame(0),
                        mainThread.frame(1).location.lineNumber - lineNumOffset,
                        methodExitEvt.returnValue.toString,
                        mainThread.frame(1).location.sourceName)
                    }
                    else {
                      //if (methodExitEvt.location.sourceName != "scripteditor"){ break;}
                      val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(
                        desc,
                        false,
                        mainThread.frame(0),
                        methodExitEvt.location.lineNumber - lineNumOffset,
                        methodExitEvt.returnValue.toString,
                        methodExitEvt.location.sourceName
                      )
                    }
                  }
                  catch {
                    case abs: AbsentInformationException =>
                      println("potatos all around")
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
      case t: Throwable => //System.err.println(s"[Exception] -- ${t.getMessage}")
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

  def isFromWrapper(stkfrm: StackFrame): Boolean = {stkfrm.thisObject().toString().contains("TracingBuiltins")}
  
  def runTurtleMethod(name: String, stkfrm: StackFrame, localArgs: List[LocalVariable]) {
    var stdTurtle = isFromWrapper(stkfrm)
    import builtins.Tw
    import builtins.TSCanvas
    var turtle: Turtle = Tw.getTurtle
    
    if (!stdTurtle) {
      var caller = stkfrm.thisObject().uniqueID()
      /*
      println("@#@#@#: " + caller)
      println("@#@#@#: " + turtlesRefs)
      */
      var index = turtlesRefs.indexOf(caller)
      
      turtle = turtles(index)
    }
      
    
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
      case "cleari" =>
		TSCanvas.cleari()
      case "invisible" =>
        turtle.invisible
      case "forward" =>
        val step = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.forward(step)
        println("@#@#@#@# YOUR THREAD IS " + mainThread.name())
        runningInBG = true
      case "right" =>
        if (localArgs.length == 0) {
          turtle.right()
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          turtle.right(angle)
        }
      case "left" =>
        if (localArgs.length == 0) {
          turtle.left()
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          turtle.left(angle)
        }
      case "back" =>
      	val step = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.back(step)
      case "home" =>
        turtle.home
      case "jumpTo" => 
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.jumpTo(x, y)
      case "setPosition" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.setPosition(x, y)
      case "setPenColor" =>
        val colorVal = stkfrm.getValue(localArgs(0)).asInstanceOf[ObjectReference]
        val mthd = colorVal.referenceType.methodsByName("toString")(0)
        val rtrndValue = colorVal.invokeMethod(mainThread, mthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
        var str = rtrndValue.asInstanceOf[StringReference].value()
        val pattern = new Regex("\\d{1,3}")
        var rgb = Vector[Int]()
        val colors = (pattern findAllIn str).foreach(c => rgb = rgb :+ c.toInt)
        val color = new Color(rgb(0),rgb(1),rgb(2))
        //println(s"Returned color string: $str")
        //println(s"Returned color raw string: ${rtrndValue.toString}")
        turtle.setPenColor(color)
      case "setFillColor" =>
        val colorVal = stkfrm.getValue(localArgs(0)).asInstanceOf[ObjectReference]
        val mthd = colorVal.referenceType.methodsByName("toString")(0)
        val rtrndValue = colorVal.invokeMethod(mainThread, mthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
        var str = rtrndValue.asInstanceOf[StringReference].value()
        val pattern = new Regex("\\d{1,3}")
        var rgb = Vector[Int]()
        val colors = (pattern findAllIn str).foreach(c => rgb = rgb :+ c.toInt)
        val color = new Color(rgb(0),rgb(1),rgb(2))
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
        turtle.circle(r)
      case "newTurtle" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtles = turtles :+ TSCanvas.newTurtle(x, y, "/images/turtle32.png")
      case "runInBackground" =>
        runningInBG = true
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
  
  def addThreadReqs(thread: ThreadReference) {
	val evtReqMgr = thread.virtualMachine().eventRequestManager
	
    val mthdEnterVal = evtReqMgr.createMethodEntryRequest()
    mthdEnterVal.addThreadFilter(thread)
    mthdEnterVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdEnterVal.enable()
  }
    
  def WatchThreadStarts() {
    val evtReqMgr = mainThread.virtualMachine().eventRequestManager
			  
    val thrdStartVal = evtReqMgr.createThreadStartRequest()
	thrdStartVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
	//thrdStartVal.addThreadFilter(mainThread)
	thrdStartVal.enable()
  }

  def createRequests(excludes: Array[String], vm: VirtualMachine) {
    val evtReqMgr = vm.eventRequestManager

    val mthdEnterVal = evtReqMgr.createMethodEntryRequest()
    excludes.foreach { mthdEnterVal.addClassExclusionFilter(_) }
    mthdEnterVal.addThreadFilter(mainThread)
    mthdEnterVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdEnterVal.enable()

    val mthdExitVal = evtReqMgr.createMethodExitRequest()
    excludes.foreach { mthdExitVal.addClassExclusionFilter(_) }
    mthdExitVal.addThreadFilter(mainThread)
    mthdExitVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdExitVal.enable()
  }

}