package no.skytteren.chart.interpolate

import no.skytteren.chart.scale.OutputRange
import no.skytteren.chart.{Color, RGB}
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

      def i(c: RGB) = Interpolater.color(OutputRange(Color.grey(0), Color.grey(255))).unapply(c).get

      assert(i(Color.grey(0)) === 0.0)
      assert(i(Color.grey(128)) === (0.5 +- 0.005))
      assert(i(Color.grey(255)) === 1.0)

    }
  }



}
