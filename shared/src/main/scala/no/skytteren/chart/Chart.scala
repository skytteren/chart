package no.skytteren.chart

import no.skytteren.chart.interpolate.Interpolater.Factory
import no.skytteren.chart.scale.{StartEndRange, _}
import no.skytteren.chart.scheme.ColorScheme
import scalatags.generic
import scala.language.higherKinds

trait Chart {

  type Builder
  type FragT
  type Output <: FragT
  type Bundle <: generic.Bundle[Builder,Output,FragT]
  val bundle : Bundle

  import bundle.all._
  import bundle.{svgTags => <}
  import bundle.{svgAttrs => ^}

  def line[D: LineChartInfo, OR[R] <: OutputRange[R]](
     data: D, width: Int, height: Int, colors: OR[RGB] = OutputRange(ColorScheme.`400`)
   )(implicit factory: Factory[RGB, OR]) = {
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
    val color = scale.Ordinal(info.map(_._1), colors)

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

  def scatterPlot[D: PlotChartInfo, OR[R] <: OutputRange[R]](
       data: D, width: Int, height: Int, colors: OR[RGB] = OutputRange(ColorScheme.`400`)
    )(implicit factory: Factory[RGB, OR]) = {
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
    val color = scale.Ordinal(info.map(_._1), colors)
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

  def barVertical[D: BarChartInfo, OR[R] <: OutputRange[R]](
    data: D, width: Int, height: Int, colors: OR[RGB] = OutputRange(ColorScheme.`400`)
  )(implicit factory: Factory[RGB, OR]) = {
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
    val color = scale.Ordinal(info.map(_._1), colors)

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y, 10),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k, v) =>
            val c = color(k)
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

  def barVerticalStacked[D: BarChartStackedInfo, OR[R] <: OutputRange[R]](
     data: D, width: Int, height: Int, colors: OR[RGB] = OutputRange(ColorScheme.`400`)
    )(implicit factory: Factory[RGB, OR]) = {
    object margin{
      val top = 20
      val right = 30
      val bottom = 30
      val left = 40
    }

    val info = implicitly[BarChartStackedInfo[D]].list(data)

    val x = scale.Ordinal(info.map(_._1), OutputRange(0, width), .1)
    val maxValue: Double = info.map(_._2.map(_._2).sum).max
    val colorId: Seq[String] = info.flatMap(_._2.map(_._1)).distinct
    val y = scale.Linear(InputRange(0d, maxValue), OutputRange(0d, height))
    val color = scale.Ordinal(colorId, colors)


    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y, 10),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.map{
          case (k1, values) =>
            values.foldLeft((0d, List[Frag]())){
              case ((offset, elems), (k2, ve)) =>
                val c = color(k2)

                (
                  offset + ve,
                  <.rect(
                    ^.`class` := "bar",
                    ^.fill := s"rgb(${c.r}, ${c.g}, ${c.b})",
                    ^.x := x(k1),
                    ^.y := height - y(offset + ve),
                    ^.height := y(ve),
                    ^.width := x.rangeBand.getOrElse(0d)
                  ) :: elems
                )
            }
        }.flatMap(_._2)
      )
    )
  }

  def barHorizontal[D: BarChartInfo, OR[R] <: OutputRange[R]](data: D, width: Int, height: Int, colors: OR[RGB] = OutputRange(ColorScheme.`400`)
                                                             )(implicit factory: Factory[RGB, OR]) = {
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
    val color = scale.Linear(InputRange(0, values.size), colors)

    <.svg(
      ^.height := height + margin.top + margin.bottom,
      ^.width := width + margin.left + margin.right,
      <.g(
        ^.transform := "translate(" + margin.left + "," + margin.top + ")",
        Axis.yLeft(y),
        Axis.xBottom(x).apply(^.transform := s"translate(0,$height)"),
        info.zipWithIndex.map{
          case ((k, v), i) =>
            val c = color(i)
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

  def donutChart[D: PieChartInfo, OR[R] <: OutputRange[R]](data: D, width: Int, height: Int, colors: OR[RGB] = OutputRange(ColorScheme.`400`)
                                 )(implicit factory: Factory[RGB, OR]) = {

    val info = implicitly[PieChartInfo[D]].list(data)
    val total = info.map(_._2).sum

    val radius = 150
    val outerRadius = radius + 50
    val donutWidth = 40

    val color = scale.Linear(InputRange(0, info.size), colors)

    val preparedInfo: List[(String, Double, Double, Int)] = info.foldLeft((Nil: List[(String, Double, Double, Int)], 0)) {
      case ((Nil, i), (label, size)) => List((label, 0d, size / total * radius * 2 * math.Pi, i)) -> (i + 1)
      case ((list @ (_, hStart, hSize, _) :: _, i), (label, size)) => ((label, hStart + hSize, size / total * radius * 2 * math.Pi, i) :: list) -> (i + 1)
    }._1.sortBy(_._1)

    <.svg(
      ^.width := width, ^.height := height, ^.viewBox := "0 0 500 500", cls := "donut",
      <.g(
        cls := "slices",
        ^.transform := "translate(" + 100 + "," + 100 + ")",
        <.circle(cls := "donut-hole", ^.cx := 150, ^.cy := 150, ^.r := radius, ^.fill := "#fff"),
        <.circle(cls := "donut-ring", ^.cx := 150, ^.cy := 150, ^.r := radius, ^.fill := "transparent", ^.stroke :="#d2d3d4", ^.strokeWidth := donutWidth),
        preparedInfo.map { case (label, start, size, i) =>
          val c = color(i)
          <.circle(
            cls := "donut-segment", ^.cx := 150, ^.cy := 150, ^.r := radius, ^.fill := "transparent",
            ^.stroke := s"rgb(${c.r}, ${c.g}, ${c.b})", ^.strokeWidth := donutWidth,
            ^.strokeDasharray := s"$size ${radius * 2 * math.Pi - size}", ^.strokeDashoffset := -start,
            label
          )
        }
      ),
      <.g(
        cls := "labels",
        preparedInfo.map { case (label, start, size, i) =>
          val mid = (start + size / 2) / radius
          val left = mid > math.Pi / 2 && mid <  3 * math.Pi / 2
          <.g(
            <.text(
              ^.x := (250 + math.cos(mid) * outerRadius),
              ^.y := (250 + math.sin(mid) * outerRadius),
              ^.dy := "-0.32em",
              ^.dx := (if(left) "-.32em" else ".32em"),
              ^.textAnchor := (if(left) "end" else "start"),
              label
            ),
            <.line(
              ^.strokeLinecap := "round",
              ^.x1 := (250 + math.cos(mid) * (donutWidth / 2 + radius)),
              ^.y1 := (250 + math.sin(mid) * (donutWidth / 2 + radius)),
              ^.x2 := (250 + math.cos(mid) * outerRadius),
              ^.y2 := (250 + math.sin(mid) * outerRadius),
              ^.stroke := "#222",
              ^.strokeWidth := 2,
            ),
            <.line(
              ^.strokeLinecap := "round",
              ^.x1 := (250 + math.cos(mid) * outerRadius),
              ^.y1 := (250 + math.sin(mid) * outerRadius),
              ^.x2 := ((if(left) 200 else 300) + math.cos(mid) * outerRadius),
              ^.y2 := (250 + math.sin(mid) * outerRadius),
              ^.stroke := "#222",
              ^.strokeWidth := 2,
            )
          )
        }
      )
    )
  }

  object Axis {

    def yLeft[D:InputData, G: NumberData](scale: Linear[D, G, StartEndRange], count: Int = 10) = {
      val number = implicitly[InputData[G]]
      val tickList: List[D] = scale.ticks(count)
      <.g(
        ^.`class` := "axis axis--y",
        ^.fill := "none",
        ^.textAnchor := "end",
        <.path(
          ^.`class` := "domain",
          ^.stroke := "#000",
          ^.d := s"M-6,${scale.outputRange.span.ceil.toInt}.5H0.5V0.5H-6"
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

    def yLeft[D, G: InputData](scale: Ordinal[D, G, StartEndRange]) = {
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

    def xBottom[D, G: InputData](scale: Ordinal[D, G, StartEndRange]) = {
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