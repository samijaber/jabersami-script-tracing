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
package net.kogics.kojo
package lite

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import javax.swing.BoxLayout
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JTextArea

import net.kogics.kojo.lite.topc.TraceHolder
import net.kogics.kojo.util.Utils

class TracingGUI(scriptEditor: ScriptEditor, kojoCtx: core.KojoCtx) {
  val events: JPanel = new JPanel
  events.setLayout(new BoxLayout(events, BoxLayout.Y_AXIS))
  val traceHolder = new TraceHolder(events)
  traceHolder.setCloseable(true)
  traceHolder.setMaximizable(false)
  traceHolder.setMinimizable(false)
  traceHolder.setExternalizable(false)

  var eventDesc: JTextArea = _
  var eventHolder: JSplitPane = _

  def reset() {
    events.removeAll()
    eventDesc = new JTextArea("Click on an Event to see its details.")
    eventDesc.setEditable(false)

    if (eventHolder != null) {
      traceHolder.remove(eventHolder)
    }

    eventHolder = new JSplitPane(JSplitPane.VERTICAL_SPLIT, new JScrollPane(events), new JScrollPane(eventDesc))
    eventHolder.setDividerLocation(500)
    eventHolder.setOneTouchExpandable(true)

    traceHolder.add(eventHolder)
    kojoCtx.makeTraceWindowVisible(traceHolder)
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
    else {
      println(taText)
    }
  }
}