package net.kogics.kojo
package lite

import java.awt.{ Color => JColor }
import java.awt.{ Font => JFont }
import java.awt.Paint

import net.kogics.kojo.core.RichTurtleCommands
import net.kogics.kojo.util.Utils

import javax.swing.JButton
import javax.swing.JFrame
import javax.swing.JScrollPane
import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode

object TracingBuiltins extends RichTurtleCommands {

  val primitives: Set[Class[_]] = Set(
    java.lang.Boolean.TYPE,
    java.lang.Character.TYPE,
    java.lang.Byte.TYPE,
    java.lang.Short.TYPE,
    java.lang.Integer.TYPE,
    java.lang.Long.TYPE,
    java.lang.Float.TYPE,
    java.lang.Double.TYPE
  )

  def inspectx[T](obj: T, name: String) {}
  def inspect[T](obj: T) { inspectx(obj, obj.toString) }
  //  lazy val kojoCtx = new NoOpKojoCtx
  //  lazy val spriteCanvas = new SpriteCanvas(kojoCtx)
  lazy val spriteCanvas = new NoOpSCanvas

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

  def epochTimeMillis = System.currentTimeMillis()
  def epochTime = System.currentTimeMillis() / 1000.0

  class Costume {
    val car = "/media/costumes/car.png"
    val pencil = "/media/costumes/pencil.png"
    val bat1 = "/media/costumes/bat1-a.png"
    val bat2 = "/media/costumes/bat1-b.png"
    val womanWaving = "/media/costumes/womanwaving.png"
  }

  class Background {
    val trainTrack = "/media/backgrounds/train-tracks3.gif"
  }

  class Sound {
    val medieval1 = "/media/music-loops/Medieval1.mp3"
  }

  val Costume = new Costume
  val Background = new Background
  val Sound = new Sound

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

  val hueMod = Utils.hueMod _
  val satMod = Utils.satMod _
  val britMod = Utils.britMod _

  lazy val turtle0 = spriteCanvas.turtle0

  def playMp3Loop(mp3File: String) {
  }

  def color(R: Int, B: Int, G: Int): Color = new Color(R, B, G)
  def Color(R: Int, B: Int, G: Int): Color = new Color(R, B, G)
  def Color(R: Int, B: Int, G: Int, a: Int): Color = new Color(R, B, G, a)
  def setBackground(c: Paint) {}

  /* movement */
  def savePosHe() {}
  def restorePosHe() {}
  def clear() {}
  def cleari() {}
  def invisible() {}
  def forward(n: Double) {}
  def forward(): Unit = forward(25)
  def back(): Unit = back(25)
  def circle(r: Double) {}
  def turn(n: Double) {}
  def home() {}
  def jumpTo(x: Double, y: Double) {}
  def moveTo(x: Double, y: Double) {}
  def setPosition(x: Double, y: Double) {}
  def setPenColor(color: Paint) {}
  def setFillColor(color: Paint) {}
  def setAnimationDelay(d: Long) {}
  def setPenThickness(d: Double) {}
  def penDown() {}
  def penUp() {}
  def repeat(n: Int)(fn: => Unit) {
    var i = 0
    while (i < n) {
      fn
      i += 1
    }
  }

  def repeatWhile(condition: Boolean)(fn: => Unit) {
    while (condition) {
      fn
    }
  }

  def changePosition(x: Double, y: Double) {}
  def scaleCostume(a: Double) {}
  def setCostumes(costumes: Vector[String]) {}

  def axesOn() {}
  def axesOff() {}
  def gridOn() {}
  def gridOff() {}
  def zoom(x: Double, y: Double, z: Double) {}

  def stopActivity() = {}
  def pause(secs: Double) = Thread.sleep((secs * 1000).toLong)
  /* turtle creation */

  def newTurtle(x: Double, y: Double): core.Turtle = {
    newTurtle(x, y, "/images/turtle32.png")
  }

  def newTurtle(x: Double, y: Double, costume: String): core.Turtle = {
    new net.kogics.kojo.turtle.Turtle(spriteCanvas, costume, x, y) {
      override def act(fn: Turtle => Unit) {
        fn(this)
      }
    }
  }

  def runInBackground(code: => Unit) { code }
}