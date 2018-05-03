package no.skytteren.chart

trait BarChartInfo[-T] {

  def list(data: T): Seq[(String, Double)]

}

object BarChartInfo{
  implicit def StringNumbericBarChartInfo[A: Numeric] = new BarChartInfo[Seq[(String, A)]]{
    val numericA = implicitly[Numeric[A]]
    override def list(data: Seq[(String, A)]): Seq[(String, Double)] = data.map{case (s, i) => (s, numericA.toDouble(i))}
  }

}
