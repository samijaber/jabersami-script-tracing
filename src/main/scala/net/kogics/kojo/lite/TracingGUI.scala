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
  
  def isOver() = ended = true
  def addChild(c : event) = {c.setParent(this); subcalls :+ c}
  def setParent(p: event) = parent = p
}

object TracingGUI extends SimpleSwingApplication {
  var box: BoxPanel = _
  var main = new event()
  var events = main.subcalls
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
        
        //set parent of new event
        lastEvent.ended match {
          case true  => newEvt.parent = lastEvent.parent
          case false => newEvt.parent = lastEvent
        }
        lastEvent = newEvt
        
      case "exit" =>
        if (lastEvent.ended)
          {lastEvent = lastEvent.parent}  
        lastEvent.isOver()
        lastEvent.exit = prompt
    }    
    
    box.contents += new TextField{
      for (i <- 1 to getLength(lastEvent))
        text += "#"
      text += prompt 
      editable = false
      }

    box.visible = false
    box.visible = true
  } 
}