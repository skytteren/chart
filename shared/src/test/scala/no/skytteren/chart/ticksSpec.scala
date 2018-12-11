package no.skytteren.chart


import org.scalatest._

class ticksSpec extends FunSpec{

  describe("ticksSpec") {
    it("should scale") {
      assert(ticks.apply(  0,  1, 10) === List(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
      assert(ticks.apply(  0,  1,  9) === List(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
      assert(ticks.apply(  0,  1,  8) === List(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
      assert(ticks.apply(  0,  1,  7) === List(0.0,      0.2,      0.4,      0.6,      0.8,      1.0))
      assert(ticks.apply(  0,  1,  6) === List(0.0,      0.2,      0.4,      0.6,      0.8,      1.0))
      assert(ticks.apply(  0,  1,  5) === List(0.0,      0.2,      0.4,      0.6,      0.8,      1.0))
      assert(ticks.apply(  0,  1,  4) === List(0.0,      0.2,      0.4,      0.6,      0.8,      1.0))
      assert(ticks.apply(  0,  1,  3) === List(0.0,                     0.5,                     1.0))
      assert(ticks.apply(  0,  1,  2) === List(0.0,                     0.5,                     1.0))
      assert(ticks.apply(  0,  1,  1) === List(0.0,                                              1.0))
      assert(ticks.apply(  0, 10, 10) === List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(ticks.apply(  0, 10,  9) === List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(ticks.apply(  0, 10,  8) === List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(ticks.apply(  0, 10,  7) === List(0,    2,    4,    6,    8,    10))
      assert(ticks.apply(  0, 10,  6) === List(0,    2,    4,    6,    8,    10))
      assert(ticks.apply(  0, 10,  5) === List(0,    2,    4,    6,    8,    10))
      assert(ticks.apply(  0, 10,  4) === List(0,    2,    4,    6,    8,    10))
      assert(ticks.apply(  0, 10,  3) === List(0,             5,             10))
      assert(ticks.apply(  0, 10,  2) === List(0,             5,             10))
      assert(ticks.apply(  0, 10,  1) === List(0,                            10))
      assert(ticks.apply(-10, 10, 10) === List(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10))
      assert(ticks.apply(-10, 10,  9) === List(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10))
      assert(ticks.apply(-10, 10,  8) === List(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10))
      assert(ticks.apply(-10, 10,  7) === List(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10))
      assert(ticks.apply(-10, 10,  6) === List(-10,       -5,       0,      5,     10))
      assert(ticks.apply(-10, 10,  5) === List(-10,       -5,       0,      5,     10))
      assert(ticks.apply(-10, 10,  4) === List(-10,       -5,       0,      5,     10))
      assert(ticks.apply(-10, 10,  3) === List(-10,       -5,       0,      5,     10))
      assert(ticks.apply(-10, 10,  2) === List(-10,                 0,             10))
      //assert(ticks.ticks(-10, 10,  1) === List(                     0               ))

    }

    it("tick increment") {
      assert(ticks.tickStep(0, 1, 10) === 0.1)
      assert(ticks.tickStep(0, 1, 9) === 0.1)
      assert(ticks.tickStep(0, 1, 8) === 0.1)
      assert(ticks.tickStep(0, 1, 7) === 0.2)
      assert(ticks.tickStep(0, 1, 6) === 0.2)
      assert(ticks.tickStep(0, 1, 5) === 0.2)
      assert(ticks.tickStep(0, 1, 4) === 0.2)
      assert(ticks.tickStep(0, 1, 3) === 0.5)
      assert(ticks.tickStep(0, 1, 2) === 0.5)
      assert(ticks.tickStep(0, 1, 1) === 1.0)
      assert(ticks.tickStep(0, 10, 10) === 1)
      assert(ticks.tickStep(0, 10, 9) === 1)
      assert(ticks.tickStep(0, 10, 8) === 1)
      assert(ticks.tickStep(0, 10, 7) === 2)
      assert(ticks.tickStep(0, 10, 6) === 2)
      assert(ticks.tickStep(0, 10, 5) === 2)
      assert(ticks.tickStep(0, 10, 4) === 2)
      assert(ticks.tickStep(0, 10, 3) === 5)
      assert(ticks.tickStep(0, 10, 2) === 5)
      assert(ticks.tickStep(0, 10, 1) === 10)
      assert(ticks.tickStep(-10, 10, 10) === 2)
      assert(ticks.tickStep(-10, 10, 9) === 2)
      assert(ticks.tickStep(-10, 10, 8) === 2)
      assert(ticks.tickStep(-10, 10, 7) === 2)
      assert(ticks.tickStep(-10, 10, 6) === 5)
      assert(ticks.tickStep(-10, 10, 5) === 5)
      assert(ticks.tickStep(-10, 10, 4) === 5)
      assert(ticks.tickStep(-10, 10, 3) === 5)
      assert(ticks.tickStep(-10, 10, 2) === 10)
      assert(ticks.tickStep(-10, 10, 1) === 20)
    }

    it("tick increment special cases") {

      assert(ticks.tickStep(3, 27, 10) === 2)
    }
  }


}
