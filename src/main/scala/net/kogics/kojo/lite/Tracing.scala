package net.kogics.kojo.lite

import scala.util.control.Breaks._
import scala.collection.JavaConversions._
import com.sun.jdi.connect._
import com.sun.jdi.event.MethodExitEvent
import com.sun.jdi.request._
import com.sun.jdi.Bootstrap
import com.sun.jdi.StringReference
import com.sun.jdi.ThreadReference
import com.sun.jdi.VirtualMachine
import com.sun.jdi.event.VMDisconnectEvent
import com.sun.jdi.event.EventQueue
import com.sun.jdi.event.EventSet
import com.sun.jdi.event.MethodEntryEvent
import java.io._
import scala.tools.nsc.Settings
import scala.tools.nsc.Global
import scala.tools.nsc.reporters._

import scala.reflect.runtime._
import scala.reflect.internal.util._
import scala.tools.reflect.ToolBox

class Tracing {
  
  var evtSet: EventSet = _
  var initconn: LaunchingConnector = _
  var mainThread: ThreadReference = _
  var codeFile: BatchSourceFile = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()  
  val compiler = new Global(settings) 

  def makeSettings() = {
      val iSettings = new Settings()
      iSettings.usejavacp.value = true
      iSettings.outputDirs.setSingleOutput(tmpdir)
      iSettings
    }

  def compile(code0: String, stopPhase: List[String] = List("cleanup")) = {
	val code = "object Wrapper { def main(args: Array[String]) { } \n" + code0 + "}"
    codeFile = new BatchSourceFile("scripteditor", code)
	val run = new compiler.Run
    run.compileSources(List(codeFile))
  }  

 
  def getVM(initconn : LaunchingConnector) = {
    var connector = initconn

    val conns = Bootstrap.virtualMachineManager().allConnectors();
    breakable { 
      for (conn <- conns) { 
      if (conn.name().equals("com.sun.jdi.CommandLineLaunch"))
        {
          connector = conn.asInstanceOf[LaunchingConnector]
          break
        }
      }
    }

    // get connector field for program's main() method
    val connArgs = connector.defaultArguments();
    val mArgs = connArgs.get("main").asInstanceOf[Connector.Argument]
    if (mArgs == null)
     throw new Error("Bad launching connector");
    // concatenate all tracer's input args into a single string
    val sb = new StringBuffer();
    sb.append(" Wrapper ")
    mArgs.setValue(sb.toString()); // assign args to main field
    
    val opts = connArgs.get("options").asInstanceOf[Connector.Argument]
    var optionValue = "-classpath " + tmpdir
	opts.setValue(optionValue)

    val vm = connector.launch(connArgs)
    vm
  }
 
 /* Printing to file is unnecessary now (No need for .scala file, only compiled .class file is needed for tracing)
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try { op(p) } finally { p.close() }
}
  
def createWrapper(code: String) {
  val data = Array("object Wrapper { \n def main(args: Array[String]) {} \n", code, "}")
  printToFile(new File("Wrapper.scala"))(p => {
	  data.foreach(p.println)
  })
}
  */
  
def trace(code: String){
  //create the file
  //createWrapper(code)
  
  val result = compile(code, Nil)
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
  allThrds.foreach { x => if (x.name == "main") mainThread = x}
  
  breakable { while (true) {
    evtSet = evtQueue.remove()
	for (evt <- evtSet.eventIterator) {
		evt match {
		case methodEnterEvt: MethodEntryEvent => 
		try {
			//Locate current stackframe, find get value of 'n' variable
			val frame = mainThread.frame(0)
					val n = methodEnterEvt.method().arguments()(0)
					val argval = frame.getValue(n)
					println("Method Enter Event (arg n): " + argval)
		}
		catch {
		case _: Throwable => println("Method Enter Event: " + methodEnterEvt.method().name)
		}
		case methodExitEvt: MethodExitEvent   => println("Method Exit Event (return value): " + methodExitEvt.returnValue)
		case vmDcEvt: VMDisconnectEvent		  => println("VM Disconnected"); break
		case _                                => println("Other")
		}
	}
	evtSet.resume()
  }
}}

  def createRequests(excludes : Array[String], vm: VirtualMachine) {
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