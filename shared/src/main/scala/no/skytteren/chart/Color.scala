package no.skytteren.chart

trait Color:
  def r: Int
  def g: Int
  def b: Int
  def a: Double
case class RGBA(r: Int, g: Int, b: Int, a: Double)
case class RGB(r: Int, g: Int, b: Int) extends Color:
  override val a = 1

object Color:

  def rgba(r: Int, g: Int, b: Int, a: Double): RGBA = RGBA(r, g, b, a)
  def rgb(r: Int, g: Int, b: Int): RGB = RGB(r, g, b)

  def apply(r: Int, g: Int, b: Int): RGB = RGB(r, g, b)
  def apply(r: Int, g: Int, b: Int, a: Double): RGBA = RGBA(r, g, b, a)

  def grey(c: Int): RGB = Color(c,c,c)
  def grey(c: Int, a: Double): RGBA = Color(c,c,c, a)

  def hex(in: String): RGB =
      val Spliter = "(.{2})(.{2})(.{2})".r
      val Spliter(r, g, b) = in
      rgb(
        Integer.parseInt(r, 16),
        Integer.parseInt(g, 16),
        Integer.parseInt(b, 16)
      )

