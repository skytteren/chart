package no.skytteren.chart

import no.skytteren.chart.ChartsServer.data
import no.skytteren.chart.scale.{Domain, GraphRange}
import scalatags.Text.all._
import scalatags.Text.tags2

object ChartView {
  def chart: Frag =
    html(
      head(tags2.title("")),
      body(
        Charts.barHorizontal(data, 500, 400),
        Charts.barVertical(data, 500, 400),
        barHorizontalLog(data, 500, 400),
        barHorizontalPow(data, 500, 400),
      )
    )


  def barHorizontalLog[D: BarChartInfo](data: D, width: Int, height: Int) = {
    import scalatags.Text.{svgTags => <}
    import scalatags.Text.{svgAttrs => ^}
    object margin{
      val top = 20
      val right = 50
      val bottom = 30
      val left = 80
    }

    val info = implicitly[BarChartInfo[D]].list(data)

    val values = info.map(_._2)
    val x = scale.Log(Domain(1, values.max), GraphRange(0, width))
    val y = scale.Ordinal(info.map(_._1), GraphRange(0, height), .1)

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Charts.Axis.yLeft(y),
        Charts.Axis.xBottom(x, 3).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k, v) =>
            <.rect(
              ^.`class` := "bar",
              ^.x := 0,
              ^.y := y(k),
              ^.height := y.rangeBand,
              ^.width := x(v)
            )
        }
      )
    )
  }

  def barHorizontalPow[D: BarChartInfo](data: D, width: Int, height: Int) = {
    import scalatags.Text.{svgTags => <}
    import scalatags.Text.{svgAttrs => ^}
    object margin{
      val top = 20
      val right = 50
      val bottom = 30
      val left = 80
    }

    val info = implicitly[BarChartInfo[D]].list(data)

    val values = info.map(_._2)
    val x = scale.Power(Domain(0, values.max), GraphRange(0, width), exponent = 2)
    val y = scale.Ordinal(info.map(_._1), GraphRange(0, height), .1)

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Charts.Axis.yLeft(y),
        Charts.Axis.xBottom(x, 5).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k, v) =>
            <.rect(
              ^.`class` := "bar",
              ^.x := 0,
              ^.y := y(k),
              ^.height := y.rangeBand,
              ^.width := x(v)
            )
        }
      )
    )
  }

}
