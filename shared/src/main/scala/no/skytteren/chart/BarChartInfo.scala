package no.skytteren.chart

trait BarChartInfo[-T] {

  def list(data: T): Seq[(String, Double)]

}

object BarChartInfo{
  implicit object StringLongBarChartInfo extends BarChartInfo[Seq[(String, Long)]]{
    override def list(data: Seq[(String, Long)]): Seq[(String, Double)] = data.map{case (s, i) => (s, i.toDouble)}
  }

  implicit object StringIntBarChartInfo extends BarChartInfo[Seq[(String, Int)]]{
    override def list(data: Seq[(String, Int)]): Seq[(String, Double)] = data.map{case (s, i) => (s, i.toDouble)}
  }

  implicit object StringDoubleBarChartInfo extends BarChartInfo[Seq[(String, Double)]]{
    override def list(data: Seq[(String, Double)]): Seq[(String, Double)] = data
  }
}
