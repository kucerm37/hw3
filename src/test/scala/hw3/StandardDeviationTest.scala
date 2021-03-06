package hw3

import hw3.Main.standardDeviation
import org.scalatest.{FunSuite, Matchers}

class StandardDeviationTest extends FunSuite with Matchers {
  test("stddev example")     {
    /**
     * x	                 4      9    11   12    17     5     8    12    14
     * (x - avg(x))^2   38.7  1.49  0.60  3.16  45.9  27.3  4.94  3.16  14.3
     *
     * sum:    sum(x) = 139.55
     * avg:    139.55/9 = 15.51
     * stddev: sqrt(15.51) = 3.94
     */
    standardDeviation(List(4, 9, 11, 12, 17, 5, 8, 12, 14)) shouldBe 3.93 +- 0.01
  }
  test("stddev singleton example")     {
    /**
     * x	                  42
     * (x - avg(x))^2        0
     * stddev: sqrt(0) = 0
     */
    standardDeviation(List(42)) shouldBe 0.0
  }
  test("passing null example") {
    assertThrows[IllegalArgumentException](standardDeviation(null))
  }

  test("passing empty vector example") {
    assertThrows[IllegalArgumentException](standardDeviation(List.empty))
  }

  test("{1, 2, -2, 4, -3}") {
    standardDeviation(List(1, 2, -2, 4, -3)) shouldBe 2.5768 +- 0.01
  }

  test("{3,5,1,8,9,-2,4,0,-10}") {
    standardDeviation(List(3,5,1,8,9,-2,4,0,-10)) shouldBe 5.4160 +- 0.01
  }

  test("empty list should throw an exception") {
    assertThrows[IllegalArgumentException](standardDeviation(List()))
  }

  test("null should throw an exception") {
    assertThrows[IllegalArgumentException](standardDeviation(null))
  }
}
