package net.kogics.kojo
package lite

import java.awt.Paint
import java.awt.{ Color => JColor }
import java.awt.{ Font => JFont }

object TracingBuiltins {
  
  type Turtle = core.Turtle
  type Color = java.awt.Color
  type Font = java.awt.Font
  type Point = core.Point
  val Point = core.Point
  type PolyLine = kgeom.PolyLine
  val PolyLine = kgeom.PolyLine
  type Point2D = java.awt.geom.Point2D.Double
  def Point2D(x: Double, y: Double) = new java.awt.geom.Point2D.Double(x, y)

  val Random = new java.util.Random

  val blue = JColor.blue
  val red = JColor.red
  val yellow = JColor.yellow
  val green = JColor.green
  val orange = JColor.orange
  val purple = new Color(0x740f73)
  val pink = JColor.pink
  val brown = new Color(0x583a0b)
  val black = JColor.black
  val white = JColor.white
  val gray = JColor.gray
  val lightGray = JColor.lightGray
  val darkGray = JColor.darkGray
  val magenta = JColor.magenta
  val cyan = JColor.cyan
  
  val BoldFont = JFont.BOLD
  val PlainFont = JFont.PLAIN
  val ItalicFont = JFont.ITALIC

  val C = staging.KColor
  //  val Color = staging.KColor
  val noColor = C.noColor

  val Kc = new staging.KeyCodes
//  val Costume = new Tw.Costume
//  val Background = new Tw.Background
//  val Sound = new Tw.Sound
  
  /* movement */
  def clear() {}
  def forward(n: Double) {}
  def right() {}
  def right(n: Double) {}
  def left() {}
  def left(n: Double) {}
  def back() {}
  def back(n: Double) {}
  def home() {}
  def jumpTo(x: Double, y: Double) {}
  def moveTo(x: Double, y: Double) {}
  def setPosition(x: Double, y: Double) {}
  def setPenColor(color: Paint) {}
  
  def repeat(n: Int) (fn: => Unit) {
    var i = 0
    while(i < n) {
      fn
      i += 1
    }
  }
}