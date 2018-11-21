package no.skytteren.chart

trait Color{
  def r: Int
  def g: Int
  def b: Int
  def a: Double
}
case class RGBA(r: Int, g: Int, b: Int, a: Double)
case class RGB(r: Int, g: Int, b: Int) extends Color{
  override val a = 1
}

object Color extends{

  def rgba(r: Int, g: Int, b: Int, a: Double) = RGBA(r, g, b, a)
  def rgb(r: Int, g: Int, b: Int) = RGB(r, g, b)

  def apply(r: Int, g: Int, b: Int) = RGB(r, g, b)
  def apply(r: Int, g: Int, b: Int, a: Double) = RGBA(r, g, b, a)

  def grey(c: Int) = Color(c,c,c, 1)
  def grey(c: Int, a: Double) = Color(c,c,c, a)

  def hex(in: String): Color = {
      val Spliter = "(.{2})(.{2})(.{2})".r
      val Spliter(r, g, b) = in
      rgb(
        Integer.parseInt(r, 16),
        Integer.parseInt(g, 16),
        Integer.parseInt(b, 16)
      )
  }
}

