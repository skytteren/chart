package no.skytteren.chart.interpolate

import no.skytteren.chart.{NumberData}
import no.skytteren.chart.scale.{Color, GraphRange}

trait Interpolater[N]{
  def apply(value: Double): N

  def unapply(value: N): Option[Double]

  def clamp(value: N): N

  def span: Option[Double]
}

object Interpolater{
  type Value = Double

  type Factory[N] = GraphRange[N] => Interpolater[N]

  implicit def numberFactory[N: NumberData]: Factory[N] = range => new InterpolateNumber[N](range)
  implicit def colorFactory: Factory[Color] = range => new InterpolateColor(range)

  def number[N: NumberData](range: GraphRange[N]): Interpolater[N] = new InterpolateNumber[N](range)
  def round[N: NumberData](range: GraphRange[N]): Interpolater[N] = new InterpolateRound[N](range)
}

case class InterpolateNumber[N: NumberData](range: GraphRange[N]) extends Interpolater[N] {
  private val numeric = implicitly[NumberData[N]]
  def apply(value: Double): N = {
    numeric.reverse(numeric(range.start) * (1 - value) + numeric(range.end) * value)
  }

  override def unapply(value: N): Option[Double] = Some((numeric(value) - numeric(range.start)) / (numeric(range.end) - numeric(range.start)))

  override def clamp(value: N): N = numeric.reverse(math.min(math.max(numeric(range.start), numeric(value)), numeric(range.end)))

  def span = Some(numeric(range.end) - numeric(range.start))
}

case class InterpolateRound[N: NumberData](range: GraphRange[N]) extends Interpolater[N]{
  private val numeric = implicitly[NumberData[N]]
  def apply(value: Double): N = {
    numeric.reverse((numeric(range.start) * (1 - value) + numeric(range.end) * value).round)
  }

  def span = Some(numeric(range.end) - numeric(range.start))

  override def unapply(value: N): Option[Double] = Some((numeric(value) - numeric(range.start)) / (numeric(range.end) - numeric(range.start)))

  override def clamp(value: N): N = numeric.reverse(math.min(math.max(numeric(range.start), numeric(value)), numeric(range.end)))

}
case class InterpolateColor(range: GraphRange[Color]) extends Interpolater[Color]{
  private def c(f: Color => Int) = Interpolater.round(GraphRange(f(range.start), f(range.end)))
  val r = c(_.r)
  val g = c(_.g)
  val b = c(_.b)
  def apply(value: Double): Color = Color(r(value), g(value), b(value))

  override def unapply(c: Color): Option[Double] = for{
    rc <- r.unapply(c.r)
    gc <- g.unapply(c.g)
    bc <- b.unapply(c.b)
  } yield {
    (rc + gc + bc) / 3
  }

  override def clamp(c: Color): Color = Color(
    r.clamp(c.r),
    g.clamp(c.g),
    b.clamp(c.b)
  )

  override def span: Option[Double] = None
}
