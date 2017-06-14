package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
/**
 * In this Lab you will implement a Specs2 testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a Specs2 specification to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class Specs2ExerciseTest extends Specification{
  "Specs2ExerciseTest" should {
    "happily flow" in {
      val e = new Euro(1, 21) / 11
      e.euro ==== 0
      e.cents ==== 11
    }

    "alternatively flow" in {
      new Euro(1, 2) / 0 must throwAn[IllegalArgumentException]
    }
  }
}
