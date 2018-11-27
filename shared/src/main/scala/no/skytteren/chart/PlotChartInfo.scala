package no.skytteren.chart

trait PlotChartInfo[-T] {

  def list(data: T): Seq[(String, Seq[(Double, Double)])]

}

object PlotChartInfo{
  implicit def stringNumberNumberPlotChartInfo[A: Numeric, B: Numeric] = new PlotChartInfo[(String, Seq[(A, B)])]{
    val numericA = implicitly[Numeric[A]]
    val numericB = implicitly[Numeric[B]]
    override def list(data: (String, Seq[(A, B)])): Seq[(String, Seq[(Double, Double)])] = {
      List(data._1 -> data._2.map{case (a, b) => numericA.toDouble(a) -> numericB.toDouble(b)})
    }
  }

  implicit def listStringNumberNumberPlotChartInfo[A: Numeric, B: Numeric] = new PlotChartInfo[Seq[(String, Seq[(A, B)])]]{
    val itemMapper = stringNumberNumberPlotChartInfo[A, B]
    override def list(data: Seq[(String, Seq[(A, B)])]): Seq[(String, Seq[(Double, Double)])] = {
      data.flatMap(itemMapper.list)
    }
  }

}
