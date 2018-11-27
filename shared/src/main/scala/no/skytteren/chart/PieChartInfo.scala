package no.skytteren.chart

trait PieChartInfo[-T] {
  def list(data: T): Seq[(String, Double)]
}

object PieChartInfo{
  implicit object StringLongPieChartInfo extends PieChartInfo[List[(String, Long)]]{
    override def list(data: List[(String, Long)]): Seq[(String, Double)] = data.map{case (s, i) => (s, i.toDouble)}
  }

  implicit object StringIntPieChartInfo extends PieChartInfo[List[(String, Int)]]{
    override def list(data: List[(String, Int)]): Seq[(String, Double)] = data.map{case (s, i) => (s, i.toDouble)}
  }

  implicit object StringDoublePieChartInfo extends PieChartInfo[List[(String, Double)]]{
    override def list(data: List[(String, Double)]): Seq[(String, Double)] = data
  }
}
