package no.skytteren.chart.scale

import no.skytteren.chart.interpolate.Interpolater
import no.skytteren.chart.{InputData, NumberData}


case class InputRange[N: InputData](start: N, end: N){
  val n = implicitly[InputData[N]]
}
object InputRange{
  def apply[N: NumberData](seq: Iterable[N]): InputRange[N] = {
    val n = implicitly[NumberData[N]]
    if(seq.isEmpty){
      InputRange(n.zero, n.zero)
    } else {
      val values = seq.map(i => i -> n(i)).toVector
      InputRange(values.minBy(_._2)._1, values.maxBy(_._2)._1)
    }
  }
}
case class OutputRange[D](start: D, end: D){
  def span(implicit number: NumberData[D]): Double = number(end) - number(start)
}
case class Color(r: Int, g: Int, b: Int)
object Color extends ((Int, Int, Int) => Color){
  def grey(c: Int) = Color(c,c,c)
}

trait Scale[D, G]{
  def apply(t: D): G
  def inverse(r: G): Option[D]

}

trait Continuous[D, G] extends Scale[D, G]{
  def interpolater: Interpolater[G]

  def clamp: Boolean

  def inputRange: InputRange[D]
  def outputRange: OutputRange[G]

  def weight(value: D): Double
  def unweight(value: Double): D

  def apply(t: D): G = {
    if(clamp){
      interpolater.clamp(interpolater(weight(t)))
    } else
      interpolater(weight(t))
  }

  def span(implicit number: NumberData[G]): Double = outputRange.span

  def ticks(count: Int = 10): List[D]

  private[scale] def ticks(start: Double, stop: Double, count: Int): List[Double] = {
    val e10 = Math.sqrt(50)
    val e5 = Math.sqrt(10)
    val e2 = Math.sqrt(2)

    def tickIncrement(start: Double, stop: Double, count: Int): Double = {
      val step: Double = (stop - start) / math.max(1, count)
      val power: Double = math.floor(math.log(step) / math.log(10))
      val error: Double = step / math.pow(10, power)
      val value = if (error >= e10) 10 else if (error >= e5) 5 else if (error >= e2) 2 else 1

      if (power >= 0)
        value * math.pow(10, power)
      else
        math.pow(10, -power) / value
    }

    if (start == stop && count > 0) {
      List(start)
    } else {
      val step = if(stop < start) -tickIncrement(stop, start, count) else tickIncrement(start, stop, count)
      if (step == 0 || step.isInfinity) {
        Nil
      } else {
        if (step > 0) {
          val startInc = Math.floor(start / step) * count
          val stopInc = step * count + 1
          startInc.to(stopInc, step).toList
        } else {
          val startInc = Math.floor(-start / step) * count
          val stopInc = step * count + 1
          startInc.to(stopInc, step).toList
        }
      }
    }
  }
} // (Identity)

trait Sequential
trait Quantize
trait Quantile
trait Threshold

case class Linear[D, G](inputRange: InputRange[D],
                        outputRange: OutputRange[G],
                        clamp: Boolean = false,
                               )(implicit interpolateFactory: Interpolater.Factory[G]) extends Continuous[D, G]{
  val interpolater = interpolateFactory(outputRange)
  private val n = inputRange.n
  val domainSpan = n(inputRange.end) - n(inputRange.start)
  def weight(value: D): Double = (n(value) - n(inputRange.start)) / domainSpan
  def unweight(value: Double): D = n.reverse(value * domainSpan + n(inputRange.start))

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  override def ticks(count: Int): List[D] = ticks(inputRange.n(inputRange.start), inputRange.n(inputRange.end), count).map(inputRange.n.reverse)
}


case class Log[D, G](inputRange: InputRange[D],
                     outputRange: OutputRange[G],
                     clamp: Boolean = false,
                     base: Double = 10
                       )(implicit interpolateFactory: Interpolater.Factory[G]) extends Continuous[D, G]{
  val interpolater = interpolateFactory(outputRange)
  private def log(v: Double) = math.log(v)/math.log(base)
  private def pow(v: Double) = math.pow(base, v)
  private val n = inputRange.n
  private val domainSpan = log(n(inputRange.end)) - log(n(inputRange.start))
  def weight(value: D): Double = log(n(value)) / domainSpan + log(n(inputRange.start))
  def unweight(value: Double): D = n.reverse(pow((value - log(n(inputRange.start))) * domainSpan))

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  override def ticks(count: Int = 10): List[D] = {
    val start = math.min(n(inputRange.start), n(inputRange.end))
    val end = math.max(n(inputRange.start), n(inputRange.end))

    val reversed: Boolean = n(inputRange.start) > n(inputRange.end)

    val result = Stream.from(0, 1).map(i => pow(i)).dropWhile(_ / start < 1).takeWhile(_ / end < base).map(n.reverse).toList
    if (reversed) result.reverse else result
  }


}

case class Power[D, G](inputRange: InputRange[D],
                       outputRange: OutputRange[G],
                       clamp: Boolean = false,
                       exponent: Double = 1
                       )(implicit interpolateFactory: Interpolater.Factory[G]) extends Continuous[D, G]{
  val interpolater = interpolateFactory(outputRange)
  private def pow(v: Double) = math.pow(v, exponent)
  private def log(v: Double) = math.pow(v, 1 / exponent)
  private val n = inputRange.n
  private val domainSpan = pow(n(inputRange.end)) - pow(n(inputRange.start))
  override def weight(value: D): Double = pow(n(value)) / domainSpan - pow(n(inputRange.start))
  override def unweight(value: Double): D = {
    n.reverse(log((value + pow(n(inputRange.start))) * domainSpan))
  }

  override def inverse(g: G): Option[D] = {
    interpolater.unapply(g).map(unweight)
  }

  override def ticks(count: Int = 10): List[D] = ticks(n(inputRange.start), n(inputRange.end), count).map(inputRange.n.reverse)
}

//trait Ordinal //(Band, Point)
case class Ordinal[D, G](elements: Seq[D],
                         range: OutputRange[G],
                         spacing: Double = 0.0,
                         clamp: Boolean = false)(implicit interpolateFactory: Interpolater.Factory[G]) extends Scale[D, G]{
  val interpolater: Interpolater[G] = interpolateFactory(range)

  val span = interpolater.span

  val rangeBand: Option[Double] = span.map(s => (s / elements.size * (1 - spacing * 2)).floor)

  def apply(t: D): G = {
    val g = interpolater(elements.indexOf(t).toDouble / elements.size + spacing / elements.size)
    if (clamp) interpolater.clamp(g) else g
  }
  def bandWidth = span

  def inverse(r: G): Option[D] = interpolater.unapply(r)
    .map(d => d * elements.size )
    .map(_.toInt)
    .filter(elements.isDefinedAt)
    .map(elements)

}

//trait Ordinal //(Band, Point)
case class Time[D, G](domain: InputRange[D],
                      range: OutputRange[G],
                      spacing: Double = 0.0,
                      clamp: Boolean = false
                     )(implicit interpolateFactory: Interpolater.Factory[G], number: InputData[G]) extends Scale[D, G]{
  val interpolater = interpolateFactory(range)
  private val n = domain.n
  val domainSpan = n(domain.end) - n(domain.start)
  def weight(value: D): Double = (n(value) - n(domain.start)) / domainSpan
  def unweight(value: Double): D = n.reverse(value * domainSpan + n(domain.start))

  def apply(t: D): G = {
    if(clamp){
      number.reverse(math.min(math.max(number(range.start), number(interpolater(weight(t)))), number(range.end)))
    } else
      interpolater(weight(t))
  }

  val span: Double = number(range.end) - number(range.start)

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  def ticks(count: Int): List[D] = ???

}

