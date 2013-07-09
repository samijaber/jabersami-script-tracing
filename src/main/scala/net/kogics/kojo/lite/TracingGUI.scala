package net.kogics.kojo.lite

import swing._
import javax.swing._
import com.sun.jdi._
import swing.event.MouseClicked

class event {
  var ended = false
  var entry: String = _
  var exit: String = _
  var subcalls = Vector[event]()
  var parent: event = null
  var entryArgs = Vector[(LocalVariable, String)]()
  var allArgs = Vector[(LocalVariable, String, String)]()
  var dclrdVars = Vector[(LocalVariable, String)]()
  var rtrnVal: String = _
  
  def isOver() {ended = true}
  def addChild(c : event) {c.setParent(this); subcalls = subcalls :+ c}
  def setParent(p: event) {parent = p}
  def setEntryArgs(stkfrm: StackFrame, localVars: List[LocalVariable]) {localVars.foreach(x => entryArgs = entryArgs :+ (x,stkfrm.getValue(x).toString))}
  def setExitArgs(stkfrm: StackFrame, localVars: List[LocalVariable]) {localVars.foreach(x => allArgs = allArgs :+ (x, findVal(entryArgs, x), stkfrm.getValue(x).toString))}
  
  def setVars(stkfrm: StackFrame, localArgs: List[LocalVariable]) {localArgs.foreach(x => dclrdVars = dclrdVars :+ (x,stkfrm.getValue(x).toString))}
  	  
  def findVal(ls : Vector [(LocalVariable, String)], x : LocalVariable): String = { ls.head match {
    case (x,a) => a
    case _ => findVal(ls.tail, x)
  }}
}


class TracingGUI extends Frame() {
  import Frame._
  var box :BoxPanel = _
  var main: event = _
  var lastEvent: event = _

  title = "Tracing Stack"  
 
  override def closeOperation(){}
  
  def refresh(){
    box = new BoxPanel(Orientation.Vertical)
    contents = box
    main = new event()
    lastEvent = main
  }
  
  def exitVal(str : String){
    lastEvent.rtrnVal = str
  }
  
  def getLength(evt: event): Int = evt.parent match {
    case null => 0
    case _ => getLength(evt.parent) + 1 
  }
  
  def addEvent(prompt: String, evt: String, isTurtle: Boolean, stkfrm: StackFrame, localArgs: List[LocalVariable], localVars: List[LocalVariable]) {
    evt match {
      case "entry" => 
        //create new event
        var newEvt = new event()
        newEvt.entry = prompt

        //set parent of new event
        lastEvent.ended match {
          case true  => lastEvent.parent.addChild(newEvt)
          case false => lastEvent.addChild(newEvt)}
        
        lastEvent = newEvt
        newEvt.setEntryArgs(stkfrm, localArgs)       
        
      case "exit" =>
        if (lastEvent.ended)
          {lastEvent = lastEvent.parent}  
        lastEvent.isOver()
        lastEvent.exit = prompt
        lastEvent.setExitArgs(stkfrm, localArgs)
        //lastEvent.setArgs(stkfrm, localArgs)
    }    
  }
  
  def setDclrdArgs(stkfrm: StackFrame, localArgs: List[LocalVariable]){
    //lastEvent.setArgs(stkfrm, localArgs)
}
  
  def printAll() {
    def printx(evt: event) {
    box.contents += new TextArea{
      listenTo(mouse.clicks)
      reactions += {
      case e: MouseClicked => evt.allArgs.foreach(x => println("Variable \"" + x._1 + "\"\nvalue at entry: " + x._2 + "\nvalue at exit: " + x._3 + "\n"))
    		  				  evt.dclrdVars.foreach(x => println("Variable \"" + x._1 + "\"\nvalue at entry: " + x._2 + "\n"))
    		  				  println("Event return value:\n" + evt.rtrnVal + "\n\n")}

      for (i <- 1 to getLength(evt))
        text += "#"
      text += evt.entry + "\n"
      for (i <- 1 to getLength(evt))
        text += "#"
      text += evt.exit
      editable = false
      }
    
    box.visible = false
    box.visible = true

    //println(evt.subcalls.length)
    evt.subcalls.foreach(x => printx(x))  
    }
    
    printx(main)
  }
}