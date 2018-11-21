package no.skytteren.chart.scale

import java.time.{LocalDate, LocalDateTime}

import org.scalatest._
import org.scalatest.Matchers._
import _root_.no.skytteren.chart.Color

class ScaleSpec extends FunSpec{

  describe("scaleLinear"){
    it("should scale") {

      def s = Linear(InputRange(0.0, 1.0), OutputRange(10.0, 20.0))

      assert(s(-1.0) === 0)
      assert(s(0.0) === 10)
      assert(s(0.2) === 12)
      assert(s(0.5) === 15)
      assert(s(2) === 30)

    }

    it("should invert") {

      def scale = Linear(InputRange(0.0, 1.0), OutputRange(10.0, 20.0))

      def s(d: Double): Double = scale.inverse(d).get

      assert(s(0) === -1.0, "0")
      assert(s(10.0) === 0.0, "10.0")
      assert(s(12.0) === 0.2, "12.0")
      assert(s(15.0) === 0.5, "15.0")
      assert(s(30) === 2, "30")

    }

    it("should clamp") {

      def s = Linear(InputRange(0.0, 1.0), OutputRange(10.0, 20.0), clamp = true)

      assert(s(-1.0) === 10, "Clamp first")
      assert(s(0.0) === 10)
      assert(s(0.2) === 12)
      assert(s(0.5) === 15)
      assert(s(2) === 20, "Clamp last")

    }
    it("should scale upp") {

      def s = Linear(InputRange(10.0, 20.0), OutputRange(10.0, 20.0))
      assert(s(10.0) === 10)
      assert(s(12) === 12)
      assert(s(15) === 15)

    }
    it("should scale down") {

      def s = Linear(InputRange(10.0, 20.0), OutputRange(0.0, 10.0))
      assert(s(10.0) === 0)
      assert(s(12) === 2)
      assert(s(15) === 5)

    }
    it("should scale reverse") {

      def s = Linear(InputRange(0.0, 10.0), OutputRange(10.0, 0.0))
      assert(s(10.0) === 0)
      assert(s(2) === 8)
      assert(s(5) === 5)

    }
  }

  describe("scaleLog"){


    it("should scale") {

      def s = Log(InputRange(1.0, 2.0), OutputRange(0.0, 1.0))

      assert(s(0.5) === -1.0000000, "0.5")
      assert(s(1.0) ===  0.0000000, "1.0")
      assert(s(2.0) ===  1.0000000, "2.0")
      assert(s(1.5) ===  (0.5849625 +- 0.00000001), "1.5")
      assert(s(2.5) ===  (1.3219281 +- 0.00000001), "2.5")

    }

    it("should invert") {

      def scale = Log(InputRange(1.0, 2.0), OutputRange(0.0, 1.0))

      def s(d: Double): Double = scale.inverse(d).get

      assert(s(-1.0000000) === (0.5 +- 0.00000001), "-1.0000000")
      assert(s( 0.0000000) === (1.0 +- 0.00000001), "0.0000000")
      assert(s( 1.0000000) === (2.0 +- 0.00000001), "1.0000000")
      assert(s( 0.5849625) === (1.5 +- 0.00000001), "0.5849625")
      assert(s( 1.3219281) === (2.5 +- 0.00000001), "1.3219281")

    }


    it("should scale with long domain") {

      def s = Log(InputRange(1.0, 10.0), OutputRange(0.0, 1.0))

      assert(s(5) === (0.69897 +- 0.00001), "5")
    }

    it("should scale to color") {

      def s = Linear(InputRange(0.0, 1.0), OutputRange(Color(0, 0, 0), Color(255, 255, 255)))

      assert(s(0) === Color(0, 0, 0))
      assert(s(0.5) === Color(128, 128, 128))
      assert(s(1) === Color(255, 255, 255))

    }

    it("should handle ticks"){
      val ints = Log(InputRange(1, 32), OutputRange(0d, 1d), base = 2)
      assert(ints.ticks() === List(1, 2, 4, 8, 16, 32))
      val e = Log(InputRange(1.0, 32.0), OutputRange(0d, 1d), base = Math.E)
      assert(e.ticks().map(_.toString).map(i => i.substring(0, math.min(i.length, 6))) === List("1.0", "2.7182", "7.3890", "20.085", "54.598"))
    }

  }

  describe("scalePower"){


    it("should scale") {

      def s = Power(InputRange(1.0, 2.0), OutputRange(0.0, 1.0))

      assert(s(0.5) === -0.5, "0.5")
      assert(s(1.0) === 0.0, "1.0")
      assert(s(1.5) === 0.5, "1.5")
      assert(s(2.0) === 1.0, "2.0")
      assert(s(2.5) === 1.5, "2.5")


      def s2 = Power(InputRange(1.0, 0.0), OutputRange(0.0, 1.0))
      println("Power ticks " + s2.ticks())
      def s3 = Power(InputRange(100.0, -100.0), OutputRange(0.0, 1.0))
      println("Power ticks " + s3.ticks())

    }


    it("should invert") {

      def s(d: Double) = Power(InputRange(1.0, 2.0), OutputRange(0.0, 1.0)).inverse(d).get

      assert(s( 0.0) === 1.0, "0.0")
      assert(s( 0.5) === 1.5, "0.5")
      assert(s( 1.0) === 2.0, "1.0")
      assert(s( 1.5) === 2.5, "1.5")
      assert(s(-0.5) === 0.5, "-0.5")


    }

  }

  describe("scaleTime"){


    val scale = Time(InputRange(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 1, 3)), OutputRange(0.0, 1.0))
    it("should scale") {

      def s = scale

      assert(s(LocalDate.of(2000, 1, 1)) === 0.0, "LocalDate.of(2000, 0, 1)")
      assert(s(LocalDate.of(2000, 1, 2)) === 0.5, "LocalDate.of(2000, 0, 2)")
    }

    it("should invert") {

      def s(d: Double) = scale.inverse(d).get
      assert(s(0.0) === LocalDate.of(2000, 1, 1), "LocalDate.of(2000, 0, 1)")
      assert(s(1.0) === LocalDate.of(2000, 1, 3), "LocalDate.of(2000, 0, 2)")
    }

  }
  describe("scaleTime dateTime"){

    val scale = Time(
      InputRange(
        LocalDateTime.of(2000, 1, 1, 0, 0, 0),
        LocalDateTime.of(2000, 1, 3, 0, 0, 0)
      ),
      OutputRange(0.0, 1.0)
    )

    it("should scale") {

      def s = scale
      assert(s(LocalDateTime.of(2000, 1, 1, 0, 0, 0)) === 0.0, "LocalDate.of(2000, 0, 1)")
      assert(s(LocalDateTime.of(2000, 1, 2, 0, 0, 0)) === 0.5, "LocalDate.of(2000, 0, 2)")
    }


    it("should invert") {

      def s(d: Double) = scale.inverse(d).get
      assert(s(0.0) === LocalDateTime.of(2000, 1, 1, 0, 0, 0), "LocalDate.of(2000, 0, 1)")
      assert(s(1.0) === LocalDateTime.of(2000, 1, 3, 0, 0, 0), "LocalDate.of(2000, 0, 2)")
    }

  }

  describe("scaleOrdinal color"){


    val scale = Ordinal(
      0 to 100,
      OutputRange(Color(0, 0, 0), Color(255, 255, 255))
    )
    it("should scale") {

      assert(scale(0) === Color(0, 0, 0), "0")
      assert(scale(50) === Color.grey(126), "0")
      assert(scale(100) === Color.grey(252), "100")

    }


    it("should invert") {

      def s(c: Color) = scale.inverse(c).get

      assert(s(Color(0, 0, 0)) === 0, "Color(0, 0, 0)")
      assert(s(Color.grey(127)) === 50, "Color(100, 100, 100)")
      assert(s(Color(254, 254, 254)) === 100, "Color(100, 100, 100)")
    }

  }

  describe("scaleOrdinal graph with spacing"){


    val scale = Ordinal(
      1 to 100,
      OutputRange(0, 2000),
      0.25
    )
    it("should scale") {

      assert(scale(1) === 5, "0")
      assert(scale(50) === 985, "50")
      assert(scale(100) === 1985, "100")

    }


    it("should invert") {

      def s(d: Int) = scale.inverse(d).get

      assert(s(5) === 1, "5")
      assert(s(995) === 50, "995")
      assert(s(1985) === 100, "1985")

    }

  }

}
