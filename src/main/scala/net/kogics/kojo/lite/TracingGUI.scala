package net.kogics.kojo.lite

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import javax.swing.BoxLayout
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JTextArea

import net.kogics.kojo.util.Utils

class TracingGUI(scriptEditor: ScriptEditor) {
  lazy val frame = new JFrame
  var events: JPanel = _
  var eventDesc: JTextArea = _

  def reset() {
    events = new JPanel()
    events.setLayout(new BoxLayout(events, BoxLayout.Y_AXIS))

    eventDesc = new JTextArea("Click on an Event to see its details.")
    eventDesc.setEditable(false)

    val eventHolder = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(events), new JScrollPane(eventDesc))
    eventHolder.setDividerLocation(500)
    eventHolder.setOneTouchExpandable(true)

    frame.getContentPane.removeAll()
    frame.getContentPane.add(eventHolder)
    frame.setBounds(100, 100, 1200, 400)
    frame.setVisible(true)
  }

  def addEvent(me: MethodEvent, source: String) = {
    val meDesc = me.toString
    val ended = me.ended
    val uiLevel = me.level + 1
    val taText = if (me.ended) "< " * uiLevel + me.exit else "> " * uiLevel + me.entry
    val lineNum = if (me.ended) me.exitLineNum else me.entryLineNum

    if (source == "scripteditor") {
      Utils.runInSwingThread {
        val te = new JTextArea(taText)
        te.setEditable(false)
        te.setLineWrap(true)
        te.setWrapStyleWord(true)

        te.addMouseListener(new MouseAdapter {
          override def mouseClicked(e: MouseEvent) {
            eventDesc.setText(meDesc)
            scriptEditor.markTraceLine(lineNum)
          }
        })

        events.add(te)
        events.revalidate()
      }
    }
    //    else {
    //      println(taText)
    //    }
  }
}