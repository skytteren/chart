package no.skytteren.chart.scale

import no.skytteren.chart.interpolate.Interpolater
import no.skytteren.chart.{DomainData, GraphData, NumberData, TimeData}


case class Domain[N: DomainData](start: N, end: N){
  val n = implicitly[DomainData[N]]
}
object Domain{
  def apply[N: NumberData](seq: Iterable[N]): Domain[N] = {
    val n = implicitly[NumberData[N]]
    if(seq.isEmpty){
      Domain(n.zero, n.zero)
    } else {
      val values = seq.map(i => i -> n(i)).toVector
      Domain(values.minBy(_._2)._1, values.maxBy(_._2)._1)
    }
  }
}
case class GraphRange[D](start: D, end: D){
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

  def domain: Domain[D]
  def range: GraphRange[G]

  def weight(value: D): Double
  def unweight(value: Double): D

  def apply(t: D): G = {
    if(clamp){
      interpolater.clamp(interpolater(weight(t)))
    } else
      interpolater(weight(t))
  }

  def span(implicit number: NumberData[G]): Double = range.span

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

      println(s"step: $step, power: $power, error: $error, value: $value")

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
} // (Linear, Power, Log, Identity, Time)

trait Sequential
trait Quantize
trait Quantile
trait Threshold

case class Linear[D, G](domain: Domain[D],
                        range: GraphRange[G],
                        clamp: Boolean = false,
                               )(implicit interpolateFactory: Interpolater.Factory[G]) extends Continuous[D, G]{
  val interpolater = interpolateFactory(range)
  private val n = domain.n
  val domainSpan = n(domain.end) - n(domain.start)
  def weight(value: D): Double = (n(value) - n(domain.start)) / domainSpan
  def unweight(value: Double): D = n.reverse(value * domainSpan + n(domain.start))

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  override def ticks(count: Int): List[D] = ticks(domain.n(domain.start), domain.n(domain.end), count).map(domain.n.reverse)
}


case class Log[D, G](domain: Domain[D],
                        range: GraphRange[G],
                        clamp: Boolean = false,
                        base: Int = 10
                       )(implicit interpolateFactory: Interpolater.Factory[G], number: NumberData[G]) extends Continuous[D, G]{
  val interpolater = interpolateFactory(range)
  private def log(v: Double) = math.log(v)/math.log(base)
  private def pow(v: Double) = math.pow(base, v)
  private val n = domain.n
  private val domainSpan = log(n(domain.end)) - log(n(domain.start))
  def weight(value: D): Double = log(n(value)) / domainSpan + log(n(domain.start))
  def unweight(value: Double): D = n.reverse(pow((value - log(n(domain.start))) * domainSpan))

  override def inverse(g: G): Option[D] = interpolater.unapply(g).map(unweight)

  override def ticks(count: Int = 10): List[D] = ticks(n(domain.start), n(domain.end), count).map(domain.n.reverse)
}

case class Power[D, G](domain: Domain[D],
                       range: GraphRange[G],
                       clamp: Boolean = false,
                       exponent: Double = 1
                       )(implicit interpolateFactory: Interpolater.Factory[G], number: NumberData[G]) extends Continuous[D, G]{
  val interpolater = interpolateFactory(range)
  private def pow(v: Double) = math.pow(v, exponent)
  private def log(v: Double) = math.pow(v, 1 / exponent)
  private val n = domain.n
  private val domainSpan = pow(n(domain.end)) - pow(n(domain.start))
  override def weight(value: D): Double = pow(n(value)) / domainSpan - pow(n(domain.start))
  override def unweight(value: Double): D = {
    n.reverse(log((value + pow(n(domain.start))) * domainSpan))
  }

  override def inverse(g: G): Option[D] = {
    interpolater.unapply(g).map(unweight)
  }

  override def ticks(count: Int = 10): List[D] = ticks(n(domain.start), n(domain.end), count).map(domain.n.reverse)
}

//trait Ordinal //(Band, Point)
case class Ordinal[D, G](elements: Seq[D],
                         range: GraphRange[G],
                         spacing: Double = 0.0,
                         clamp: Boolean = false)(implicit interpolateFactory: Interpolater.Factory[G]) extends Scale[D, G]{
  val interpolater: Interpolater[G] = interpolateFactory(range)

  val span = interpolater.span
  println("span : " + span)

  val rangeBand: Option[Double] = span.map(s => (s / elements.size * (1 - spacing * 2)).floor)

  def apply(t: D): G = {
    val g = interpolater(elements.indexOf(t).toDouble / elements.size + spacing / elements.size)
    if (clamp) interpolater.clamp(g) else g
  }
  def bandWidth = span

  def inverse(r: G): Option[D] = interpolater.unapply(r)
    .map(d => (d - spacing) * elements.size )
    .map(_.toInt)
    .map(i => {
      println(i)
      println(elements.isDefinedAt(i))
      i
    })
    .filter(elements.isDefinedAt)
    .map(elements)

}

//trait Ordinal //(Band, Point)
case class Time[D, G](domain: Domain[D],
                                range: GraphRange[G],
                                spacing: Double = 0.0,
                                clamp: Boolean = false
                     )(implicit interpolateFactory: Interpolater.Factory[G], timeData: TimeData[D], number: DomainData[G]) extends Scale[D, G]{
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

