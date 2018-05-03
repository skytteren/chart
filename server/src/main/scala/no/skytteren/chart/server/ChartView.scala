package no.skytteren.chart.server

import no.skytteren.chart.scale.{Domain, GraphRange}
import no.skytteren.chart.{BarChartInfo, Charts, scale}
import scalatags.Text.all._
import scalatags.Text.tags2

import scala.util.Random

object ChartView {

  def data = List(
    {"Locke"    -> math.random() * 100},
    {"Reyes"    -> math.random() * 100},
    {"Ford"     -> math.random() * 100},
    {"Jarrah"   -> math.random() * 100},
    {"Shephard" -> math.random() * 100},
    {"Kwon"     -> math.random() * 100}
  )

  def dataList = Seq(
    "A" ->  Seq(
      1 -> math.random() * 5,
      2 -> math.random() * 4,
      3 -> math.random() * 2,
      4 -> math.random() * 6,
      5 -> math.random() * 6,
      6 -> math.random() * 5,
      7 -> math.random() * 8,
    ),
    "B" ->  Seq(
      2 -> math.random() * 15,
      3 -> math.random() * 14,
      4 -> math.random() * 12,
      5 -> math.random() * 11,
      6 -> math.random() * 16,
      7 -> math.random() * 15,
      8 -> math.random() * 18,
    ),
    "C" ->  Seq(
      1 -> (math.random() * 10 + 0),
      3 -> (math.random() * 10 + 5),
      4 -> (math.random() * 10 + 2),
      5 -> (math.random() * 10 + 1),
      6 -> (math.random() * 10 + 2),
      7 -> (math.random() * 10 + 4),
      8 -> (math.random() * 10 + 6),
    )
  )

  def randomList: Seq[(String, Seq[(Double, Double)])] = Seq("A", "B", "C").map(_ ->
    (0 to 100).map(_ =>
      Random.nextGaussian() * 20 -> math.pow(math.random(), 2) * 20
    ).toSeq
  )

  def list(list: Seq[String]): Frag = {
    html(
      head(tags2.title("Chart")),
      body(
        h1("Charts"),
        ul(
          list.map( item =>
            li(a(href := s"/html/$item", item), " - ", a(href := s"/$item", "svg"))
          ),
          li(a(href := s"/examples", "examples"))
        )
      )
    )
  }

  def chart(name: String, chart: Frag): Frag =
  html(
    head(tags2.title("Chart")),
    body(
      h1(name),
      chart
    )
  )

  def examples: Frag =
    html(
      head(tags2.title("Chart")),
      body(
        Charts.scatterPlot(randomList, 500, 400),
        Charts.scatterPlot(dataList, 500, 400),
        Charts.line(dataList, 500, 400),
        Charts.barHorizontal(data, 500, 400),
        Charts.barVertical(data, 500, 400),
        barHorizontalLog(data, 500, 400),
        barHorizontalPow(data, 500, 400),
      )
    )


  def barHorizontalLog[D: BarChartInfo](data: D, width: Int, height: Int) = {
    import scalatags.Text.{svgAttrs => ^, svgTags => <}
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
        Charts.Axis.xBottom(x, 10).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k, v) =>
            <.rect(
              ^.`class` := "bar",
              ^.x := 0,
              ^.y := y(k),
              ^.height := y.rangeBand.getOrElse(0d),
              ^.width := x(v)
            )
        }
      )
    )
  }

  def barHorizontalPow[D: BarChartInfo](data: D, width: Int, height: Int) = {
    import scalatags.Text.{svgAttrs => ^, svgTags => <}
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
              ^.height := y.rangeBand.getOrElse(0d),
              ^.width := x(v)
            )
        }
      )
    )
  }

}
