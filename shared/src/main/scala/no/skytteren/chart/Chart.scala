package no.skytteren.chart

import no.skytteren.chart.scale._

import scalatags.generic

trait Chart {

  type Builder
  type FragT
  type Output <: FragT
  type Bundle <: generic.Bundle[Builder,Output,FragT]
  val bundle : Bundle

  import bundle.all._
  import bundle.{svgTags => <}
  import bundle.{svgAttrs => ^}

  def line[D: LineChartInfo](data: D, width: Int, height: Int) = {
    object margin{
      val top = 20
      val right = 30
      val bottom = 30
      val left = 40
    }

    val info = implicitly[LineChartInfo[D]].list(data)

    val points = info.flatMap(_._2)
    val x = scale.Linear(InputRange(points.map(_._1)), OutputRange(0, width))
    val y = scale.Linear(InputRange(points.map(_._2)), OutputRange(0, height))
    val color = scale.Ordinal(info.map(_._1), OutputRange(Color(250, 25, 25), Color(55, 250, 50)))

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y, 10),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.map { case (lineName, linePoints) =>
          val c = color(lineName)
          <.path(
            ^.`class` := "line",
            ^.d := linePoints
              .map { case (k, v) => s"${x(k)},${height - y(v)}" }
              .mkString("M", "L", ""),
            ^.style := s"stroke: rgb(${c.r}, ${c.g}, ${c.b}); opacity: 1; fill: none"
          )
        },
        info.map { case (lineName, linePoints) =>
          val c = color(lineName)
          linePoints.map { case (k, v) =>
            <.circle(
              ^.`class` := "circle",
              ^.cx := x(k),
              ^.cy := height - y(v),
              ^.r := "2.5",
              ^.style := s"fill: rgb(${c.r}, ${c.g}, ${c.b}); opacity: 1;"
            )

          }
        }
      )
    )
  }

  def scatterPlot[D: PlotChartInfo](data: D, width: Int, height: Int) = {
    object margin{
      val top = 20
      val right = 30
      val bottom = 30
      val left = 40
    }

    val info = implicitly[PlotChartInfo[D]].list(data)

    val points = info.flatMap(_._2)
    val x = scale.Linear(InputRange(points.map(_._1)), OutputRange(0, width))
    val y = scale.Linear(InputRange(points.map(_._2)), OutputRange(0, height))
    val color = scale.Ordinal(info.map(_._1), OutputRange(Color(50, 25, 25), Color(55, 250, 250)))
    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y, 10),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.map { case (lineName, classPoints) =>
          val c = color(lineName)
          classPoints.map { case (k, v) =>
            <.circle(
              ^.`class` := "circle",
              ^.cx := x(k),
              ^.cy := height - y(v),
              ^.r := "2.5",
              ^.style := s"fill: rgb(${c.r}, ${c.g}, ${c.b}); opacity: 1;"
            )
          }
        }
      )
    )
  }

  def barVertical[D: BarChartInfo](data: D, width: Int, height: Int) = {
    object margin{
      val top = 20
      val right = 30
      val bottom = 30
      val left = 40
    }

    val info = implicitly[BarChartInfo[D]].list(data)

    val x = scale.Ordinal(info.map(_._1), OutputRange(0, width), .1)
    val values = info.map(_._2)
    val y = scale.Linear(InputRange(0, values.max), OutputRange(0, height))
    val color = scale.Linear(InputRange(0, values.max), OutputRange(Color(250, 25, 25), Color(55, 250, 50)))

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y, 10),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k, v) =>
            val c = color(v)
            <.rect(
              ^.`class` := "bar",
              ^.fill := s"rgb(${c.r}, ${c.g}, ${c.b})",
              ^.x := x(k),
              ^.y := height - y(v),
              ^.height := y(v),
              ^.width := x.rangeBand.getOrElse(0d)
            )
        }
      )
    )
  }

  def barHorizontal[D: BarChartInfo](data: D, width: Int, height: Int) = {
    object margin{
      val top = 20
      val right = 50
      val bottom = 30
      val left = 80
    }

    val info = implicitly[BarChartInfo[D]].list(data)

    val values = info.map(_._2)
    val x = scale.Linear(InputRange(0, values.max), OutputRange(0, width))
    val y = scale.Ordinal(info.map(_._1), OutputRange(0, height), .1)
    val color = scale.Linear(InputRange(0, values.max), OutputRange(Color(0, 25, 25), Color(255, 50, 50)))

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k, v) =>
            val c = color(v)
            <.rect(
              ^.`class` := "bar",
              ^.fill := s"rgb(${c.r}, ${c.g}, ${c.b})",
              ^.x := 0,
              ^.y := y(k),
              ^.height := y.rangeBand.getOrElse(0d),
              ^.width := x(v)
            )
        }
      )
    )
  }

  object Axis {

    def yLeft[D:InputData, G: NumberData](scale: Linear[D, G], count: Int = 10) = {
      val number = implicitly[InputData[G]]
      val tickList: List[D] = scale.ticks(count)
      <.g(
        ^.`class` := "axis axis--y",
        ^.fill := "none",
        ^.textAnchor := "end",
        <.path(
          ^.`class` := "domain",
          ^.stroke := "#000",
          ^.d := s"M-6,${scale.span.ceil.toInt}.5H0.5V0.5H-6"
        ),
        tickList.map(tick => {
          <.g(
            ^.`class` := "tick",
            ^.opacity := "1",
            ^.transform := s"translate(0,${(number(scale.outputRange.end) - number(scale(tick))).round.toInt}.5)",
            <.line(
              ^.stroke := "#000",
              ^.x2 := "-6"
            ),
            <.text(
              ^.fill := "#000",
              ^.x := "-9",
              ^.dy := "0.32em",
              tick.toString
            )
          )
        }),
        <.text(
          ^.transform := "rotate(-90)",
          ^.fill := "#000",
          ^.y := "6",
          ^.dy := "0.71em",
          ^.textAnchor := "end",
          "Frequency"
        )
      )
    }

    def yLeft[D, G: InputData](scale: Ordinal[D, G]) = {
      val number = implicitly[InputData[G]]
      <.g(
        ^.`class` := "axis axis--y",
        ^.fill := "none",
        ^.textAnchor := "end",
        <.path(
          ^.`class` := "domain",
          ^.stroke := "#000",
          ^.d := s"M-6,${scale.span.getOrElse(0d).ceil.toInt}.5H0.5V0.5H-6"
        ),
        scale.elements.map(e => {
          <.g(
            ^.`class` := "tick",
            ^.opacity := "1",
            ^.transform := s"translate(0,${(number(scale(e)) + scale.rangeBand.getOrElse(0d) / 2).toInt}.5)",
            <.line(
              ^.stroke := "#000",
              ^.x2 := "-6"
            ),
            <.text(
              ^.fill := "#000",
              ^.x := "-9",
              ^.dy := "0.32em",
              e.toString
            )
          )
        })

      )
    }

    def xBottom[D, G: InputData](scale: Ordinal[D, G]) = {
      val number = implicitly[InputData[G]]
      <.g(
        ^.`class` := "axis axis--x",
        ^.fill := "none",
        ^.textAnchor := "middle",
        <.path(
          ^.`class` := "domain",
          ^.stroke := "#000",
          ^.d := s"M0.5,6V0.5H${scale.span.getOrElse(0d).ceil.toInt}.5"
        ),
        scale.elements.map(e => {
          <.g(
            ^.`class` := "tick",
            ^.opacity := "1",
            ^.transform := s"translate(${number(scale(e)) + scale.rangeBand.getOrElse(0d) / 2},0)",
            <.line(
              ^.stroke := "#000",
              ^.y2 := "6"
            ),
            <.text(
              ^.fill := "#000",
              ^.y := "9",
              ^.dy := "0.71em",
              e.toString
            )
          )
        }),
        <.text(
          ^.transform := "rotate(-90)",
          ^.y := "6",
          ^.dy := "0.71em",
          ^.textAnchor := "middle",
          "Frequency"
        )
      )
    }

    def xBottom[D, G: InputData](scale: Continuous[D, G], count: Int = 10) = {
      val tickList: List[D] = scale.ticks(count)
      val number = implicitly[InputData[G]]
      <.g(
        ^.`class` := "axis axis--x",
        ^.fill := "none",
        ^.textAnchor := "middle",
        <.path(
          ^.`class` := "domain",
          ^.stroke := "#000",
          ^.d := s"M0.5,6V0.5H${number(scale(tickList.last)).ceil.toInt}.5"
        ),
        tickList.map(tick => {
          <.g(
            ^.`class` := "tick",
            ^.opacity := "1",
            ^.transform := s"translate(${number(scale(tick)).round.toInt}.5,0)",
            <.line(
              ^.stroke := "#000",
              ^.y2 := "6"
            ),
            <.text(
              ^.fill := "#000",
              ^.y := "9",
              ^.dy := "0.71em",
              tick.toString
            )
          )
        }),
        <.text(
          ^.transform := "rotate(-90)",
          ^.y := "6",
          ^.dy := "0.71em",
          ^.textAnchor := "middle",
          "Frequency"
        )
      )
    }

  }

}