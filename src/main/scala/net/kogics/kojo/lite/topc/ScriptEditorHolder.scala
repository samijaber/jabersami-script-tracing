package net.kogics.kojo.lite.topc

import java.awt.BorderLayout
import java.awt.Color
import java.awt.Point
import java.awt.event.ActionEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import javax.swing.AbstractAction
import javax.swing.JCheckBoxMenuItem
import javax.swing.JMenuItem
import javax.swing.JPanel
import javax.swing.JPopupMenu
import javax.swing.KeyStroke
import javax.swing.event.PopupMenuEvent
import javax.swing.event.PopupMenuListener

import org.fife.ui.autocomplete.AutoCompletion
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea
import org.fife.ui.rsyntaxtextarea.RSyntaxTextAreaEditorKit.IncreaseFontSizeAction
import org.fife.ui.rsyntaxtextarea.Style
import org.fife.ui.rsyntaxtextarea.SyntaxConstants
import org.fife.ui.rsyntaxtextarea.TokenTypes
import org.fife.ui.rsyntaxtextarea.templates.StaticCodeTemplate
import org.fife.ui.rtextarea.RTextScrollPane

import net.kogics.kojo.codingmode.SwitchMode
import net.kogics.kojo.lite.CodeExecutionSupport
import net.kogics.kojo.lite.KojoCompletionProvider
import net.kogics.kojo.livecoding.IpmProvider
import net.kogics.kojo.util.Utils
import net.kogics.kojo.xscala.CodeTemplates

import scalariform.formatter.ScalaFormatter

class ScriptEditorHolder(val se: JPanel, codePane: RSyntaxTextArea, codeSupport: CodeExecutionSupport) extends BaseHolder("SE", "Script Editor", se) {

  codeSupport.toolbar.setOpaque(true)
  codeSupport.toolbar.setBackground(new Color(230, 230, 230))

  codePane.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SCALA)
  codePane.setAntiAliasingEnabled(true)
  codePane.setAnimateBracketMatching(false)
  codePane.setCloseCurlyBraces(true)
  codePane.setTabsEmulated(true)
  codePane.setTabSize(4)
  //  codePane.setMarkOccurrences(true)
  codePane.getSyntaxScheme.setStyle(TokenTypes.SEPARATOR, new Style(Color.blue))
  new IncreaseFontSizeAction().actionPerformedImpl(null, codePane)

  RSyntaxTextArea.setTemplatesEnabled(true)
  val ctm = RSyntaxTextArea.getCodeTemplateManager()
  //  ctm.setInsertTrigger(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0))

  CodeTemplates.templates.foreach { kv =>
    val name = kv._1; val value = kv._2
    val ct = new StaticCodeTemplate(name, CodeTemplates.beforeCursor(name), CodeTemplates.afterCursor(name))
    ctm.addTemplate(ct)
  }

  val provider = new KojoCompletionProvider(codeSupport)
  val ac = new AutoCompletion(provider)
  ac.setParameterAssistanceEnabled(true)
  ac.setAutoActivationEnabled(true)
  ac.setShowDescWindow(true)
  ac.install(codePane)

  val sp = new RTextScrollPane(codePane)
  se.setLayout(new BorderLayout)
  se.add(codeSupport.toolbar, BorderLayout.NORTH)
  se.add(sp, BorderLayout.CENTER)
  se.add(codeSupport.statusStrip, BorderLayout.EAST)

  val popup = codePane.getPopupMenu
  popup.add(new JPopupMenu.Separator, 2)

  val switcher = new SwitchMode()
  val twCb = new JCheckBoxMenuItem(switcher)
  twCb.setText(Utils.loadString("S_TurtleMode"))
  twCb.setToolTipText(Utils.loadString("S_TurtleModeTT"))
  twCb.setActionCommand("Tw")
  popup.add(twCb, 3)

  val stagingCb = new JCheckBoxMenuItem(switcher)
  stagingCb.setText(Utils.loadString("S_StagingMode"))
  stagingCb.setToolTipText(Utils.loadString("S_StagingModeTT"))
  stagingCb.setActionCommand("Staging")
  popup.add(stagingCb, 4)

  val mwCb = new JCheckBoxMenuItem(switcher)
  mwCb.setText(Utils.loadString("S_MwMode"))
  mwCb.setToolTipText(Utils.loadString("S_MwModeTT"))
  mwCb.setActionCommand("Mw")
  popup.add(mwCb, 5)

  val d3Cb = new JCheckBoxMenuItem(switcher)
  d3Cb.setText(Utils.loadString("S_D3Mode"))
  d3Cb.setToolTipText(Utils.loadString("S_D3ModeTT"))
  d3Cb.setActionCommand("D3")
  popup.add(d3Cb, 6)
  
  popup.add(new JPopupMenu.Separator, 7)

  
  val formatAction = new AbstractAction("Format Source") {
    def actionPerformed(ev: ActionEvent) {
      codePane.setText(ScalaFormatter.format(codePane.getText))
    }
  }
  
  val formatItem = new JMenuItem(formatAction)
  val cst = KeyStroke.getKeyStroke("control shift F")
  val im = codePane.getInputMap()
  im.put(cst, "format-src")
  val am = codePane.getActionMap()
  am.put("format-src", formatAction)
  formatItem.setAccelerator(cst)
  popup.add(formatItem, 8)

  popup.addPopupMenuListener(new PopupMenuListener {
    def popupMenuWillBecomeVisible(e: PopupMenuEvent) {
      switcher.updateCb(twCb)
      switcher.updateCb(stagingCb)
      switcher.updateCb(mwCb)
      switcher.updateCb(d3Cb)
    }
    def popupMenuWillBecomeInvisible(e: PopupMenuEvent) {}

    def popupMenuCanceled(e: PopupMenuEvent) {}
  })

  def activate() {
    toFront()
    codePane.requestFocusInWindow()
  }

  val ipmProvider = new IpmProvider(codeSupport)
  codePane.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent) {
      try {
        val pt = new Point(e.getX(), e.getY());
        val offset = codePane.viewToModel(pt);
        if (ipmProvider.isHyperlinkPoint(codePane, offset)) {
          // ipmProvider.getHyperlinkSpan(codePane, offset)
          ipmProvider.performClickAction(codePane, offset)
        }
        else {
          codeSupport.imanip.foreach { _ close () }
        }
      }
      catch {
        case t: Throwable => println("IPM Problem: " + t.getMessage)
      }
    }
  })
}