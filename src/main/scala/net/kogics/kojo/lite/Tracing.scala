package net.kogics.kojo.lite

import java.io.File

import scala.collection.JavaConversions._
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import com.sun.jdi.Bootstrap
import com.sun.jdi.ThreadReference
import com.sun.jdi.VirtualMachine
import com.sun.jdi.connect.Connector
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.EventSet
import com.sun.jdi.event.MethodEntryEvent
import com.sun.jdi.event.MethodExitEvent
import com.sun.jdi.event.VMDisconnectEvent
import com.sun.jdi.request.EventRequest
import com.sun.jdi.AbsentInformationException
import com.sun.jdi.LocalVariable

import net.kogics.kojo.util.Utils

class Tracing {

  var evtSet: EventSet = _
  var initconn: LaunchingConnector = _
  var mainThread: ThreadReference = _
  var codeFile: BatchSourceFile = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()
  val compiler = new Global(settings)

  val wrapperCode = """object Wrapper { 
  def main(args: Array[String]) { 
    %s
  }
    
  def clear() {}
  def forward(n: Double) {}
  def right(n: Double) {}
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
    codeFile = new BatchSourceFile("scripteditor", code)
    val run = new compiler.Run
    run.compileSources(List(codeFile))
    
  }

  def makeSettings() = {
    val iSettings = new Settings()
    iSettings.usejavacp.value = true
    iSettings.outputDirs.setSingleOutput(tmpdir)
    iSettings
  }

  def getVM(initconn: LaunchingConnector) = {
    var connector = initconn

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

  def trace(code: String) = Utils.runAsync {
 
    val result = compile(code)
    //Connect to target VM
    val vm = getVM(initconn)
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
    
    val creatorGUI = TracingGUI
    val tracingFrame = creatorGUI.top
    tracingFrame.visible = true
    
    breakable {
      while (true) {
        evtSet = evtQueue.remove()
        for (evt <- evtSet.eventIterator) {
          evt match {
            case methodEnterEvt: MethodEntryEvent =>
              try {
                //Locate current stackframe, find get value of 'n' variable
                val frame = mainThread.frame(0)
                var toprint = "";
                
                if(methodEnterEvt.method().arguments().size > 0){
                	var n = methodEnterEvt.method().arguments()(0)
               		var argval = frame.getValue(n)
                	var argname = n.name
                	toprint = " (arg " + n.name + "): " + argval
                 }
                
                //determine if the method is a Turtle API method
                methodEnterEvt.method().name match {
                  case "forward" | "right" | "clear" =>                 
                    var strng = s"Method Enter Event [${mainThread.frame(1).location().lineNumber - 2}] ${methodEnterEvt.method().name}" + toprint
                    creatorGUI.addEvent(strng, "entry", true, mainThread.frame(0), methodEnterEvt.method().arguments().toList)
                  case _ =>
                    var strng = s"Method Enter Event [${methodEnterEvt.location().lineNumber - 2}] ${methodEnterEvt.method().name}" + toprint
                    try {creatorGUI.addEvent(strng, "entry", false, mainThread.frame(0), methodEnterEvt.method().arguments().toList)}
                    catch {case e: AbsentInformationException => 
                      creatorGUI.addEvent(strng, "entry", false, mainThread.frame(0), List[LocalVariable]())}
                }
              }
              catch {
                case _: Throwable => println(s"Method Enter Event [${methodEnterEvt.location().lineNumber - 2}]  " + methodEnterEvt.method().name)
              }
            case methodExitEvt: MethodExitEvent => 
              //determine if the method is a Turtle API method
                methodExitEvt.method().name match {
                  case "forward" | "right" | "clear" =>                 
                    var strng = s"Method Exit Event [${mainThread.frame(1).location().lineNumber - 2}] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                    creatorGUI.addEvent(strng, "exit", true, mainThread.frame(0), methodExitEvt.method().arguments().toList)
                  case _ =>
                    var strng = s"Method Exit Event [${methodExitEvt.location().lineNumber - 2}] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                    try {creatorGUI.addEvent(strng, "exit", false, mainThread.frame(0), methodExitEvt.method().arguments().toList)}
                    catch {case e: AbsentInformationException => 
                      creatorGUI.addEvent(strng, "exit", false, mainThread.frame(0), List[LocalVariable]())}
                      creatorGUI.exitVal(methodExitEvt.returnValue().toString())
               }
            case vmDcEvt: VMDisconnectEvent =>
              println("VM Disconnected"); break
            case _ => println("Other")
          }
        }
        evtSet.resume()
      }
    }
    creatorGUI.printAll()
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