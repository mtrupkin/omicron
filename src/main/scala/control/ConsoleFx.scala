package me.mtrupkin.control

import java.lang.Math._
import javafx.scene.control.Label
import javafx.scene.layout.{Pane, StackPane}
import javafx.scene.paint.{Paint, Color}
import javafx.scene.text.{Font, FontWeight}

import me.mtrupkin.console.{RGB, Screen, ScreenChar}
import me.mtrupkin.core.{Matrix, Point, Size}

import scala.Array._

/**
 * Created by mtrupkin on 12/13/2014.
 */
class ConsoleFx(val size: Size) extends Pane {
  setStyle("-fx-background-color: black;")
  val offsetX, offsetY = 1

  val font = Font.font("Consolas", FontWeight.NORMAL, 19)
  val charBounds = ConsoleFx.charBounds(font)
  val stacks = new Matrix[StackPane](size)
  val labels = new Matrix[Label](size)
  val (conWidth, conHeight) = toPixel(Point(size.width, size.height))
  val (sizeX, sizeY) = (conWidth + offsetX * 2, conHeight + offsetY * 2)
  setPrefSize(sizeX, sizeY)
  setMinSize(sizeX, sizeY)

  size.foreach(init)

  def init(p: Point): Unit = {
    val s = new StackPane()
    val l = new Label()


    l.setTextFill(Color.WHITE)
    l.setFont(font)

    s.getChildren.addAll(l)

    stacks(p) = s
    labels(p) = l

    val (px, py) = toPixel(p.x, p.y)
    s.relocate(px, py)
    getChildren.add(s)
  }

  def apply(p: Point): Label = labels(p)

  // draw screen to window
  def draw(screen: Screen): Unit = screen.foreach(draw)

  def draw(p: Point, s: ScreenChar): Unit = {
    val l = this(p)
    l.setText(s)
    l.setTextFill(color(s.fg))
  }

  def color(c: RGB): Color = new Color(c.r/255f, c.g/255f, c.b/255f, 1)

  def toPixel(p: Point): (Double, Double) = {
    val (width, height) = charBounds

    (p.x * width + offsetX, p.y * height + offsetY)
  }

  def floor(d: Double): Int = Math.floor(d).toInt

  def toScreen(x: Double, y: Double): Option[Point] = {
    val (width, height) = charBounds
    val c = Point(floor((x-offsetX) / width), floor((y-offsetY) / height))
    if (size.in(c)) Some(c) else None
  }

//  def pixelSize(): Size = toPixel(size)
}

object ConsoleFx {
  def charBounds(f: Font): (Double, Double) = {
    val fl = com.sun.javafx.tk.Toolkit.getToolkit.getFontLoader

    val metrics = fl.getFontMetrics(f)
    val fontWidth = fl.computeStringWidth(" ", f)
    (floor(fontWidth), floor(metrics.getLineHeight))
  }
}
