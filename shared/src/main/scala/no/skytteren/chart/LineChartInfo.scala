package no.skytteren.chart

trait LineChartInfo[T] {

  def list(data: T): Seq[(String, Seq[(Double, Double)])]

}

object LineChartInfo{
  implicit def stringNumberNumberLineChartInfo[A: Numeric, B: Numeric] = new LineChartInfo[(String, Seq[(A, B)])]{
    val numericA = implicitly[Numeric[A]]
    val numericB = implicitly[Numeric[B]]
    override def list(data: (String, Seq[(A, B)])): Seq[(String, Seq[(Double, Double)])] = {
      List(data._1 -> data._2.map{case (a, b) => numericA.toDouble(a) -> numericB.toDouble(b)})
    }
  }

  implicit def listStringNumberNumberLineChartInfo[A: Numeric, B: Numeric] = new LineChartInfo[Seq[(String, Seq[(A, B)])]]{
    val itemMapper = stringNumberNumberLineChartInfo[A, B]
    override def list(data: Seq[(String, Seq[(A, B)])]): Seq[(String, Seq[(Double, Double)])] = {
      data.flatMap(itemMapper.list)
    }
  }

}
