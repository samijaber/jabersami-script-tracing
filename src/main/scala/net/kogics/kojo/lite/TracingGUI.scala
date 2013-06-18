package net.kogics.kojo.lite

import swing._
import com.sun.jdi.ThreadReference
import com.sun.jdi.StackFrame


object TracingGUI extends SimpleSwingApplication {
  var box: BoxPanel = _
  var counter: Int = 0
  var stack: Array[StackFrame] = new Array[StackFrame](0)
  
  def top = new MainFrame {
    title = "Tracing Stack"
    box = new BoxPanel(Orientation.Vertical)
    contents = box
    counter = 0
    stack = new Array[StackFrame](0)
  }
  
  def addEvent(txt: String, prevframe: StackFrame, currframe: StackFrame) {
    counter = counter + 1
    
    box.contents += new TextField{
      text = "Event #" + counter + ":" + txt
      try { if (stack.length > 1 && stack.indexOf(prevframe) != -1)
      		{text += ". Also, this event was called from within the call at event #" + stack.indexOf(prevframe)}} 
      try { if (stack.length > 1 && stack.indexOf(currframe) != -1)
      		{text += ". Also, this event is occuring on the same level as event #" + stack.indexOf(currframe)}} 
      editable = false
      }

    stack :+ currframe
    box.visible = false
    box.visible = true
  } 
}