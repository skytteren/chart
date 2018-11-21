package no.skytteren.chart.interpolate

import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import no.skytteren.chart.{Color, NumberData, RGB}
import no.skytteren.chart.scale.OutputRange

trait Interpolater[N]{
  def apply(value: Double): N

  def unapply(value: N): Option[Double]

  def clamp(value: N): N

  def span: Option[Double]
}

object Interpolater{
  type Value = Double

  type Factory[N] = OutputRange[N] => Interpolater[N]

  implicit def numberFactory[N: NumberData]: Factory[N] = range => new InterpolateNumber[N](range)
  implicit def colorFactory: Factory[RGB] = range => new InterpolateColor(range)

  def number[N: NumberData](range: OutputRange[N]): Interpolater[N] = new InterpolateNumber[N](range)
  def round[N: NumberData](range: OutputRange[N]): Interpolater[N] = new InterpolateRound[N](range)
  def color(range: OutputRange[RGB]): Interpolater[RGB] = new InterpolateColor(range)
  def date(range: OutputRange[LocalDate]): Interpolater[LocalDate] = new InterpolateDate(range)
  def dateTime(range: OutputRange[LocalDateTime]): Interpolater[LocalDateTime] = new InterpolateDateTime(range)
}

case class InterpolateNumber[N: NumberData](range: OutputRange[N]) extends Interpolater[N] {
  private val numeric = implicitly[NumberData[N]]
  def apply(value: Double): N = {
    numeric.reverse(numeric(range.start) * (1 - value) + numeric(range.end) * value)
  }

  override def unapply(value: N): Option[Double] = Some((numeric(value) - numeric(range.start)) / (numeric(range.end) - numeric(range.start)))

  override def clamp(value: N): N = numeric.reverse(math.min(math.max(numeric(range.start), numeric(value)), numeric(range.end)))

  def span = Some(numeric(range.end) - numeric(range.start))
}

case class InterpolateRound[N: NumberData](range: OutputRange[N]) extends Interpolater[N]{
  private val numeric = implicitly[NumberData[N]]
  def apply(value: Double): N = {
    numeric.reverse((numeric(range.start) * (1 - value) + numeric(range.end) * value).round)
  }

  def span = Some(numeric(range.end) - numeric(range.start))

  override def unapply(value: N): Option[Double] = Some((numeric(value) - numeric(range.start)) / (numeric(range.end) - numeric(range.start)))

  override def clamp(value: N): N = numeric.reverse(math.min(math.max(numeric(range.start), numeric(value)), numeric(range.end)))

}

case class InterpolateColor(range: OutputRange[RGB]) extends Interpolater[RGB]{
  private def c(f: Color => Int) = Interpolater.round(OutputRange(f(range.start), f(range.end)))
  val r = c(_.r)
  val g = c(_.g)
  val b = c(_.b)
  def apply(value: Double): RGB = Color(r(value), g(value), b(value))

  override def unapply(c: RGB): Option[Double] = for{
    rc <- r.unapply(c.r)
    gc <- g.unapply(c.g)
    bc <- b.unapply(c.b)
  } yield {
    (rc + gc + bc) / 3
  }

  override def clamp(c: RGB): RGB = Color(
    r.clamp(c.r),
    g.clamp(c.g),
    b.clamp(c.b)
  )

  override def span: Option[Double] = None
}

case class InterpolateDate(range: OutputRange[LocalDate]) extends Interpolater[LocalDate]{

  override def apply(value: Double): LocalDate = LocalDate.ofEpochDay((range.start.toEpochDay * (1 - value) + range.end.toEpochDay * value).toLong)

  override def unapply(d: LocalDate): Option[Double] = span.map((d.toEpochDay - range.start.toEpochDay) / _)

  override def clamp(d: LocalDate): LocalDate = {
    if(d.isBefore(range.start)){
      range.start
    } else if (d.isAfter(range.end)) {
      range.end
    } else {
      d
    }
  }

  override def span: Option[Double] = Some(range.end.toEpochDay - range.start.toEpochDay)
}


case class InterpolateDateTime(range: OutputRange[LocalDateTime]) extends Interpolater[LocalDateTime]{

  override def apply(value: Double): LocalDateTime = LocalDateTime.ofEpochSecond(
    (range.start.toEpochSecond(ZoneOffset.UTC) * (1 - value) + range.end.toEpochSecond(ZoneOffset.UTC) * value).toLong,
    0,
    ZoneOffset.UTC
  )

  override def unapply(d: LocalDateTime): Option[Double] = span.map((d.toEpochSecond(ZoneOffset.UTC) - range.start.toEpochSecond(ZoneOffset.UTC)) / _)

  override def clamp(d: LocalDateTime): LocalDateTime = {
    if(d.isBefore(range.start)){
      range.start
    } else if (d.isAfter(range.end)) {
      range.end
    } else {
      d
    }
  }

  override def span: Option[Double] = Some(range.end.toEpochSecond(ZoneOffset.UTC) - range.start.toEpochSecond(ZoneOffset.UTC))
}