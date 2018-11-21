package no.skytteren.chart

import java.time.{LocalDate, LocalDateTime, ZoneOffset}

trait InputData[T]{
  def apply(t:T): Double
  def reverse(d: Double): T
}

trait GraphData[T, D] {
  def apply(t:T): D
  def reverse(d: D): T
}

trait GraphColor extends GraphData[Color, Color]

trait NumberData[T] extends InputData[T] with GraphData[T, Double]{
  def zero: T
}

trait TimeData[T] extends InputData[T]

object InputData{
  implicit object DoubleInputData$ extends NumberData[Double]{
    override def apply(d:Double): Double = d

    override def zero: Double = 0

    override def reverse(d: Double): Double = d
  }
  implicit object LongInputData$ extends NumberData[Long]{
    override def apply(d:Long): Double = d.toDouble

    override def zero: Long = 0L

    override def reverse(d: Double): Long = d.round
  }
  implicit object IntInputData$ extends NumberData[Int]{
    override def apply(d:Int): Double = d.toDouble

    override def zero: Int = 0

    override def reverse(d: Double): Int = d.round.toInt
  }

  implicit object LocalDateInputData$ extends TimeData[LocalDate]{
    override def apply(d:LocalDate): Double = d.toEpochDay

    override def reverse(d: Double): LocalDate = LocalDate.ofEpochDay(d.toLong)
  }

  implicit object LocalDateTimeInputData$ extends TimeData[LocalDateTime]{
    override def apply(d:LocalDateTime): Double = d.toEpochSecond(ZoneOffset.UTC)

    override def reverse(d: Double): LocalDateTime = LocalDateTime.ofEpochSecond(d.toLong, 0, ZoneOffset.UTC)
  }


}
