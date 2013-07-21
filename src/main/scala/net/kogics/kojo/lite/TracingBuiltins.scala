package net.kogics.kojo
package lite

import java.awt.Paint
import java.awt.{ Color => JColor }
import java.awt.{ Font => JFont }
import net.kogics.kojo.core._
import net.kogics.kojo.picture._
import net.kogics.kojo.util.Utils
import lite.canvas.SpriteCanvas
import net.kogics.kojo.turtle.Turtle

object TracingBuiltins {
  
  val kojoCtx = new NoOpKojoCtx
  lazy val spriteCanvas = new SpriteCanvas(kojoCtx)
  
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
  
  val hueMod = Utils.hueMod _
  val satMod = Utils.satMod _
  val britMod = Utils.britMod _
  
  var turtles = Vector[Turtle]()
  
  def color(R: Int, B: Int, G: Int): Color = new Color(R,B,G)
  def Color(R: Int, B: Int, G: Int): Color = new Color(R,B,G)
  def Color(R: Int, B: Int, G: Int, a:Int): Color = new Color(R,B,G,a)
  
  /* movement */
  def clear() {}
  def cleari(){}
  def invisible(){}
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
  def setFillColor(color: Paint){}
  def setAnimationDelay(d: Long){}
  def setPenThickness(d: Double){}
  def penDown(){}
  def penUp(){}
  def repeat(n: Int) (fn: => Unit) {
    var i = 0
    while(i < n) {
      fn
      i += 1
    }
  }
  def circle(r: Double){}
  
  def newTurtle(x: Double, y: Double): Turtle = {
    var t0 = new net.kogics.kojo.turtle.Turtle(spriteCanvas, "/images/turtle32.png", x, y) 
    turtles = turtles :+ t0
    turtles.last
    }
  
  def runInBackground(code: => Unit) = Utils.runAsyncMonitored(code)
}