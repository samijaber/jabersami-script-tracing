package net.kogics.kojo.lite

import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame

class MethodEvent {
  var ended = false
  var entry: String = _
  var exit: String = _
  var parent: Option[MethodEvent] = None
  var subcalls = Vector[MethodEvent]()
  var entryVars = Vector[(LocalVariable, String)]()
  var returnVal: String = _
  var entryLineNum: Int = _
  var exitLineNum: Int = _
  var sourceName: String = _
  //  var allVars = Vector[(LocalVariable, String, String)]()
  //  var dclrdArgs = Vector[(LocalVariable, String)]()

  override def toString() = s"""MethodEvent(
Entry: $entry
Exit: $exit
Args: ${entryVars map { vs => val lv = vs._1; s"${lv.name}: ${lv.`type`.name} = ${vs._2}" }}
Return value: $returnVal
Entry Line Number: $entryLineNum
Exit Line Number: $exitLineNum
Source: $sourceName
ended: $ended
)"""

  def isOver() { ended = true }

  def setParent(p: Option[MethodEvent]) {
    parent = p
    parent foreach { _.addChild(this) }
  }

  private def addChild(c: MethodEvent) { subcalls = subcalls :+ c }

  def setEntryVars(stkfrm: StackFrame, localVars: List[LocalVariable]) {
    localVars.foreach { x =>
      val xVal = try {
        stkfrm.getValue(x).toString
      }
      catch {
        case e: Throwable => "unavailable"
      }
      entryVars = entryVars :+ (x, xVal)
    }
  }

  //  def setExitVars(stkfrm: StackFrame, localVars: List[LocalVariable]) {
  //    localVars.foreach { x =>
  //      val xVal = findVal(entryVars, x)
  //      val xVal2 = try {
  //        stkfrm.getValue(x).toString
  //      }
  //      catch {
  //        case e: Throwable => "unavailable"
  //      }
  //      allVars = allVars :+ (x, xVal, xVal2)
  //    }
  //  }
  //
  //  def setArgs(stkfrm: StackFrame, localArgs: List[LocalVariable]) {
  //    localArgs.foreach { x =>
  //      val xVal = try {
  //        stkfrm.getValue(x).toString
  //      }
  //      catch {
  //        case e: Throwable => "unavailable"
  //      }
  //      dclrdArgs = dclrdArgs :+ (x, xVal)
  //    }
  //  }

  def findVal(ls: Vector[(LocalVariable, String)], x: LocalVariable): String = {
    ls.head match {
      case (x, a) => a
      case _      => findVal(ls.tail, x)
    }
  }

  def level: Int = parent match {
    case None    => 0
    case Some(p) => p.level + 1
  }
}
