package net.kogics.kojo.lite

import swing._
import com.sun.jdi.ThreadReference
import com.sun.jdi.StackFrame

class event {
  var ended = false
  var entry: String = _
  var exit: String = _
  var subcalls = new Array[event](0)
  var parent: event = null
  
  def isOver() {ended = true}
  def addChild(c : event) {c.setParent(this); subcalls = subcalls :+ c}
  def setParent(p: event) {parent = p}
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
  
  def getLength(evt: event): Int = evt.parent match {
    case null => 0
    case _ => getLength(evt.parent) + 1 
  }
  
  def addEvent(prompt: String, evt: String, isTurtle: Boolean) {
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
        
      case "exit" =>
        if (lastEvent.ended)
          {lastEvent = lastEvent.parent}  
        lastEvent.isOver()
        lastEvent.exit = prompt
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

    println(evt.subcalls.length)
    evt.subcalls.map(x => printx(x))  
    }
    
    printx(main)
  }
}