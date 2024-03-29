package no.skytteren.chart.scale

import no.skytteren.chart.interpolate.Interpolater
import no.skytteren.chart.{InputData, NumberData}
import Ordering.Double.TotalOrdering

case class InputRange[N: InputData](start: N, end: N):
  val n = implicitly[InputData[N]]

object InputRange:
  def apply[N: NumberData](seq: Iterable[N]): InputRange[N] =
    val n = summon[NumberData[N]]
    if seq.isEmpty then
      InputRange(n.zero, n.zero)
    else
      val values = seq.map(i => i -> n(i)).toVector
      InputRange(values.minBy(_._2)._1, values.maxBy(_._2)._1)

sealed trait OutputRange[+D] {
  //def span(implicit number: NumberData[D]): Double
  //def start: D
  //def end: D
}

case class StepsRange[D](steps: List[D]) extends OutputRange[D]:
  require(steps.size >= 2, "Not enough steps in OutputRange, got " + steps)

case class StartEndRange[D](start: D, end: D) extends OutputRange[D]:
  def span(implicit number: NumberData[D]): Double = number(end) - number(start)

object OutputRange:
  def apply[D](steps: List[D]) = new StepsRange[D](steps)

  def apply[D](start: D, end: D) = new StartEndRange[D](start, end)

  def apply[D](start: D, step1: D, step2: D, steps: D*) = new StepsRange[D](List(start, step1, step2) ++ steps.toList)

trait Scale[D, G]:
  def apply(t: D): G

  def inverse(r: G): Option[D]


trait Continuous[D, G] extends Scale[D, G]:
  def interpolater: Interpolater[G]

  def clamp: Boolean

  def inputRange: InputRange[D]

  def outputRange: OutputRange[G]

  def weight(value: D): Double

  def unweight(value: Double): D

  def apply(t: D): G =
    if clamp then
      interpolater.clamp(interpolater(weight(t)))
    else
      interpolater(weight(t))

  def ticks(count: Int = 10): List[D]

// (Identity)

trait Sequential

trait Quantize

trait Quantile

trait Threshold

case class Linear[D, G, O[N] <: OutputRange[N]](
                                              inputRange: InputRange[D],
                                              outputRange: O[G],
                                              clamp: Boolean = false,
                                            )(
                                              implicit interpolateFactory: Interpolater.Factory[G, O]
                                            ) extends Continuous[D, G]:
  val interpolater = interpolateFactory(outputRange)
  private val n = inputRange.n
  val domainSpan: Double = n(inputRange.end) - n(inputRange.start)

  def weight(value: D): Double = (n(value) - n(inputRange.start)) / domainSpan

  def unweight(value: Double): D = n.reverse(value * domainSpan + n(inputRange.start))

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  override def ticks(count: Int): List[D] = no.skytteren.chart.ticks(inputRange.n(inputRange.start), inputRange.n(inputRange.end), count).map(inputRange.n.reverse)

  def ticks2(count: Int = 10): List[D] = no.skytteren.chart.ticks.ticks2(inputRange.n(inputRange.start), inputRange.n(inputRange.end), count).map(inputRange.n.reverse)


case class Log[D, G, O[A] <: OutputRange[A]](inputRange: InputRange[D],
                                             outputRange: O[G],
                                             clamp: Boolean = false,
                                             base: Double = 10
                                            )(implicit interpolateFactory: Interpolater.Factory[G, O]) extends Continuous[D, G]:
  val interpolater = interpolateFactory(outputRange)

  private def log(v: Double) = math.log(v) / math.log(base)

  private def pow(v: Double) = math.pow(base, v)

  private val n = inputRange.n
  private val domainSpan = log(n(inputRange.end)) - log(n(inputRange.start))

  def weight(value: D): Double = log(n(value)) / domainSpan + log(n(inputRange.start))

  def unweight(value: Double): D = n.reverse(pow((value - log(n(inputRange.start))) * domainSpan))

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  override def ticks(count: Int = 10): List[D] =
    val start = math.min(n(inputRange.start), n(inputRange.end))
    val end = math.max(n(inputRange.start), n(inputRange.end))

    val reversed: Boolean = n(inputRange.start) > n(inputRange.end)

    val result = LazyList.from(0, 1).map(i => pow(i)).dropWhile(_ / start < 1).takeWhile(_ / end < base).map(n.reverse).toList
    if reversed then result.reverse else result



case class Power[D, G, O[A] <: OutputRange[A]](inputRange: InputRange[D],
                                               outputRange: O[G],
                                               clamp: Boolean = false,
                                               exponent: Double = 1
                                              )(implicit interpolateFactory: Interpolater.Factory[G, O]) extends Continuous[D, G]:
  val interpolater = interpolateFactory(outputRange)

  private def pow(v: Double) = math.pow(v, exponent)

  private def log(v: Double) = math.pow(v, 1 / exponent)

  private val n = inputRange.n
  private val domainSpan = pow(n(inputRange.end)) - pow(n(inputRange.start))

  override def weight(value: D): Double = pow(n(value)) / domainSpan - pow(n(inputRange.start))

  override def unweight(value: Double): D =
    n.reverse(log((value + pow(n(inputRange.start))) * domainSpan))

  override def inverse(g: G): Option[D] =
    interpolater.unapply(g).map(unweight)

  override def ticks(count: Int = 10): List[D] = no.skytteren.chart.ticks.apply(n(inputRange.start), n(inputRange.end), count).map(inputRange.n.reverse)

//trait Ordinal //(Band, Point)
case class Ordinal[D, G, O[A] <: OutputRange[A]](elements: Seq[D],
                                                 range: O[G],
                                                 spacing: Double = 0.0,
                                                 clamp: Boolean = false)(implicit interpolateFactory: Interpolater.Factory[G, O]) extends Scale[D, G]:
  val interpolater: Interpolater[G] = interpolateFactory(range)

  val span = interpolater.span

  val rangeBand: Option[Double] = span.map(s => (s / elements.size * (1 - spacing * 2)).floor)

  def apply(t: D): G =
    val g = interpolater(elements.indexOf(t).toDouble / elements.size + spacing / elements.size)
    if clamp then interpolater.clamp(g) else g

  def bandWidth = span

  def inverse(r: G): Option[D] = interpolater.unapply(r)
    .map(d => d * elements.size)
    .map(_.toInt)
    .filter(elements.isDefinedAt)
    .map(elements)


//trait Ordinal //(Band, Point)
case class Time[D, G](domain: InputRange[D],
                      range: StartEndRange[G],
                      spacing: Double = 0.0,
                      clamp: Boolean = false
                     )(implicit interpolateFactory: Interpolater.Factory[G, StartEndRange], number: InputData[G]) extends Scale[D, G]:
  val interpolater = interpolateFactory(range)
  private val n = domain.n
  val domainSpan = n(domain.end) - n(domain.start)

  def weight(value: D): Double = (n(value) - n(domain.start)) / domainSpan

  def unweight(value: Double): D = n.reverse(value * domainSpan + n(domain.start))

  def apply(t: D): G =
    if clamp then
      number.reverse(math.min(math.max(number(range.start), number(interpolater(weight(t)))), number(range.end)))
    else
      interpolater(weight(t))

  val span: Double = number(range.end) - number(range.start)

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  def ticks(count: Int): List[D] = ???


