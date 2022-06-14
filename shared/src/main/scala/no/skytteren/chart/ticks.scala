package no.skytteren.chart

object ticks {

  private val e10 = Math.sqrt(50d)
  private val e5 = Math.sqrt(10d)
  private val e2 = Math.sqrt(2d)
  private val ln10 = BigDecimal(Math.log(10))
  implicit class BigDecimalPimp(bd: BigDecimal):
    def ceil: BigDecimal = bd.setScale(0, BigDecimal.RoundingMode.CEILING)
    def floor: BigDecimal = bd.setScale(0, BigDecimal.RoundingMode.FLOOR)

  def apply(start: BigDecimal, stop: BigDecimal, count: Int): List[Double] =

    val reverse: Boolean = start > stop

    val min = start min stop
    val max = start max stop

    if min == max && count > 0 then
      List(start.toDouble)
    else
      val step = tickStep(min, max, count)
      val steps: List[BigDecimal] =
        if step == 0d || step.toDouble.isInfinity then
          Nil
        else
          if step > 0 then
            val startInc = (min / step).floor * step
            val stopInc = (max / step).ceil * step

            startInc.to(stopInc, step).toList
          else
            val startInc = (min * step).floor * step
            val stopInc = (max * step).ceil * step
            startInc.to(stopInc, step).toList
      (if reverse then steps.reverse else steps).map(_.toDouble)

  def increment(start: BigDecimal, stop: BigDecimal, count: Int): Double =
    val step: BigDecimal = (stop - start) / math.max(1, count)
    val power: Double = (BigDecimal(math.log(step.toDouble)) / ln10).toDouble.floor
    val error: BigDecimal = step / math.pow(10, power)
    val value: Double = if error >= e10 then 10 else if error >= e5 then 5 else if error >= e2 then 2 else 1

    if power >= 0 then
      value * math.pow(10, power)
    else
      -math.pow(10, -power) / value

  def tickStep(start: BigDecimal, stop: BigDecimal, count: Int): BigDecimal =
    val step0 = (stop - start).abs / math.max(0, count)
    val step1 = Math.pow(10, (BigDecimal(math.log(step0.toDouble)) / ln10).toDouble.floor)
    val error: BigDecimal = step0 / step1
    val stepping: Double = if error >= e10 then step1 * 10 else if error >= e5 then step1 * 5 else if error >= e2 then step1 * 2 else step1
    if stop < start then -stepping else stepping


  def ticks2(start: BigDecimal, stop: BigDecimal, count: Int): List[Double] =
    // Minimal increment to avoid round extreme values to be on the edge of the chart
    val (max, min) =
      val max = start max stop
      val min = start min stop
      val epsilon = (max - min) / 1e6
      (max + epsilon, min - epsilon)
    val range = max - min

    // Target number of values to be displayed on the Y axis (it may be less)
    val stepCount = count
    // First approximation
    val roughStep: BigDecimal = range / (stepCount - 1)

    // Set best step for the range
    val goodNormalizedSteps = List(1, 1.5, 2, 2.5, 5, 7.5, 10) // keep the 10 at the end
    // Or use these if you prefer:  { 1, 2, 5, 10 };

    // Normalize rough step to find the normalized one that fits best
    val stepPower = BigDecimal(math.pow(10, -math.floor(math.log10(roughStep.abs.toDouble))))
    val normalizedStep = roughStep * stepPower
    val goodNormalizedStep = goodNormalizedSteps.find(n => n >= normalizedStep).getOrElse(0d)
    val step = goodNormalizedStep / stepPower

    // Determine the scale limits based on the chosen step.
    val scaleMax = (max.setScale(math.max(start.scale, stop.scale), BigDecimal.RoundingMode.HALF_DOWN) / step).ceil * step
    val scaleMin = (min.setScale(math.max(start.scale, stop.scale), BigDecimal.RoundingMode.HALF_DOWN) / step).floor * step

    val ticks = scaleMin.to(scaleMax, step)
      .toList
      .map(_.toDouble)

    if start < stop then
      ticks
    else
      ticks.reverse


}