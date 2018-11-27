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

trait BarChartStackedInfo[-T] {

  def list(data: T): Seq[(String, Seq[(String, Double)])]

}

object BarChartStackedInfo{
  implicit def StringNumbericBarChartStackedInfo[A: Numeric] = new BarChartStackedInfo[Seq[(String, Seq[(String, A)])]]{
    val numericA = implicitly[Numeric[A]]
    override def list(data: Seq[(String, Seq[(String, A)])]): Seq[(String, Seq[(String, Double)])] =
      data.map{case (s, list) => (s, list.map{case (s2, i) => s2 -> numericA.toDouble(i)})}
  }

  implicit def StringIntNumbericBarChartStackedInfo[A: Numeric] = new BarChartStackedInfo[Seq[(String, Seq[(Int, A)])]]{
    val numericA = implicitly[Numeric[A]]
    override def list(data: Seq[(String, Seq[(Int, A)])]): Seq[(String, Seq[(String, Double)])] =
      data.map{case (s, list) => (s, list.map{case (s2, i) => s2.toString -> numericA.toDouble(i)})}
  }

}

