package no.skytteren.chart.interpolate

import java.time.{LocalDate, LocalDateTime}

import no.skytteren.chart.scale.OutputRange
import no.skytteren.chart.Color

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class InterpolateSpec extends FunSpec{

  describe("Interpolate number"){
    it("should interpolate") {

      def i = Interpolater.number(OutputRange(10.0, 20.0))

      assert(i(0.0) === 10)
      assert(i(0.2) === 12)
      assert(i(0.5) === 15)

    }

    it("should deinterpolate") {

      def d(i: Double) = Interpolater.number(OutputRange(10.0, 20.0)).unapply(i).get

      assert(d(10) === 0.0)
      assert(d(12) === 0.2)
      assert(d(15) === 0.5)

    }
  }
  describe("Interpolate round"){
    it("should interpolate") {

      def i = Interpolater.round(OutputRange(10.0, 20.0))

      assert(i(0.01) === 10)

      assert(i(0.201) === 12)

      assert(i(0.499) === 15)

    }
  }

  describe("Interpolate color"){
    it("should interpolate") {

      def i = Interpolater.color(OutputRange(Color.grey(0), Color.grey(255)))

      assert(i(0.0) === Color.grey(0))
      assert(i(0.5) === Color.grey(128))
      assert(i(1) === Color.grey(255))

    }

    it("should deinterpolate") {

      def i(c: Color) = Interpolater.color(OutputRange(Color.grey(0), Color.grey(255))).unapply(c).get

      assert(i(Color.grey(0)) === 0.0)
      assert(i(Color.grey(128)) === (0.5 +- 0.005))
      assert(i(Color.grey(255)) === 1.0)

    }
  }


  describe("Interpolate date"){
    it("should interpolate") {

      def i = Interpolater.date(OutputRange(LocalDate.of(2018, 1, 1), LocalDate.of(2018, 12, 31)))

      assert(i(0.0) === LocalDate.of(2018, 1, 1))

      assert(i(0.5) === LocalDate.of(2018, 7, 2))

      assert(i(1) === LocalDate.of(2018, 12, 31))

    }
  }

  describe("Interpolate datetime"){
    it("should interpolate") {

      def i = Interpolater.dateTime(OutputRange(LocalDateTime.of(2018, 1, 1, 0, 0, 0), LocalDateTime
        .of(2018, 12, 31, 23, 59, 59)))

      assert(i(0.0) === LocalDateTime.of(2018, 1, 1, 0, 0, 0))

      assert(i(0.5) === LocalDateTime.of(2018, 7, 2, 11, 59, 59))

      assert(i(1) === LocalDateTime.of(2018, 12, 31, 23, 59, 59))

    }
  }

}
