package net.kogics.kojo.lite

import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame
import scala.swing.MainFrame
import javax.swing.JFrame
import javax.swing.JList
import javax.swing.JLabel
import javax.swing.JPanel
import java.awt.FlowLayout
import javax.swing.BoxLayout
import javax.swing.JTextArea
import javax.swing.JScrollPane
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.BorderLayout

object TracingGUI {
  lazy val frame = new JFrame
  var events: JPanel = _
  var eventDesc: JTextArea = _

  def reset() {
    val eventHolder = new JPanel
    eventHolder.setLayout(new BorderLayout)
    eventHolder.add(new JLabel("<html><strong>Trace Events:</strong></html>"), BorderLayout.NORTH)
    
    events = new JPanel()
    events.setLayout(new BoxLayout(events, BoxLayout.Y_AXIS))
    events.add(new JTextArea(" " * 120))
    eventHolder.add(new JScrollPane(events), BorderLayout.WEST)

    eventDesc = new JTextArea("Click on an Event to see its details.")
    eventDesc.setEditable(false)
    eventHolder.add(eventDesc, BorderLayout.CENTER)
    
    
    frame.getContentPane.removeAll()
    frame.getContentPane.add(eventHolder)
    frame.setBounds(100, 100, 1200, 400)
    frame.setVisible(true)
  }

  def addEvent(me: MethodEvent) {
    val te = if (me.ended) {
      new JTextArea("< " * (me.level+1) + me.exit)
    }
    else {
      new JTextArea("> " * (me.level+1) + me.entry)
    }
    
    te.setEditable(false)
    
    val meDesc = me.toString
    te.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent) {
        eventDesc.setText(meDesc)
      }
    })

    events.add(te)
    events.revalidate()
  }
}