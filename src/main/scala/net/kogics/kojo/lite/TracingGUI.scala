package net.kogics.kojo.lite

import swing._
import com.sun.jdi._
import swing.event.MouseClicked

class event {
  var ended = false
  var entry: String = _
  var exit: String = _
  var subcalls = new Array[event](0)
  var parent: event = null
  var entryVars = new Array[(LocalVariable, String)](0)
  var allVars = new Array[(LocalVariable, String, String)](0)
  var rtrnVal: String = _
  
  def isOver() {ended = true}
  def addChild(c : event) {c.setParent(this); subcalls = subcalls :+ c}
  def setParent(p: event) {parent = p}
  def setEntryVars(stkfrm: StackFrame, localVars: List[LocalVariable]) {localVars.map(x => entryVars = entryVars :+ (x,stkfrm.getValue(x).toString))}
  def setExitVars(stkfrm: StackFrame, localVars: List[LocalVariable]) {localVars.map(x => allVars = allVars :+ (x, findEntryVar(entryVars, x), stkfrm.getValue(x).toString))}
  
  def findEntryVar(ls : Array[(LocalVariable, String)], x : LocalVariable): String = { ls.head match {
    case (x,a) => a
    case _ => findEntryVar(ls.tail, x)
  }}
}


object TracingGUI extends SimpleSwingApplication {
  var box: BoxPanel = _
  var main = new event()
  var lastEvent = main
  

  def top = new MainFrame {
    title = "Tracing Stack"
    box = new BoxPanel(Orientation.Vertical)
    contents = box
  }
  
  def exitVal(str : String){
    lastEvent.rtrnVal = str
  }
  
  def getLength(evt: event): Int = evt.parent match {
    case null => 0
    case _ => getLength(evt.parent) + 1 
  }
  
  def addEvent(prompt: String, evt: String, isTurtle: Boolean, stkfrm: StackFrame, localVars: List[LocalVariable]) {
    evt match {
      case "entry" => 
        //create new event
        var newEvt = new event()
        newEvt.entry = prompt
        main.addChild(newEvt)

        //set parent of new event
        lastEvent.ended match {
          case true  => lastEvent.parent.addChild(newEvt)
          case false => lastEvent.addChild(newEvt)
        }
        
        lastEvent = newEvt
        newEvt.setEntryVars(stkfrm, localVars)        
        
      case "exit" =>
        if (lastEvent.ended)
          {lastEvent = lastEvent.parent}  
        lastEvent.isOver()
        lastEvent.exit = prompt
        lastEvent.setExitVars(stkfrm, localVars)
    }    
    
/*
    box.contents += new TextArea{
      for (i <- 1 to getLength(lastEvent))
        text += "#"
      text += prompt
      editable = false
      }

    box.visible = false
    box.visible = true
*/
  }
  
  def printAll() {
    def printx(evt: event) {
    box.contents += new TextArea{
      listenTo(mouse.clicks)
      reactions += {
      case e: MouseClicked => evt.allVars.foreach(x => println(x._1 + "\n value at entry: " + x._2 + "\nvalue at exit: " + x._3 + "\n"))
    		  				  println("Event return value:\n" + evt.rtrnVal + "\n\n")
        }

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
    evt.subcalls.map(x => printx(x))  
    }
    
    printx(main)
  }
}