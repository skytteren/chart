package no.skytteren.chart.interpolate

import no.skytteren.chart.scale.GraphRange
import org.scalatest.FunSpec

class InterpolateSpec extends FunSpec{

  describe("Interpolate number"){
    it("should interpolate") {

      def i = Interpolater.number(GraphRange(10.0, 20.0))

      assert(i(0.0) === 10)

      assert(i(0.2) === 12)

      assert(i(0.5) === 15)

    }
  }
  describe("Interpolate round"){
    it("should interpolate") {

      def i = Interpolater.round(GraphRange(10.0, 20.0))

      assert(i(0.01) === 10)

      assert(i(0.201) === 12)

      assert(i(0.499) === 15)

    }
  }

}
