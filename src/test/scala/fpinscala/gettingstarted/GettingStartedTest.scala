package fpinscala.gettingstarted

import org.scalatest.FunSuite

class GettingStartedTest extends FunSuite {
  test("PolymorphicFunctions.isSorted") {
    // true case
    assert(true == PolymorphicFunctions.isSorted(Array(1, 2, 3), (x: Int, y: Int) => x <= y))
    // false case
    assert( false == PolymorphicFunctions.isSorted(Array(1.0, 3.0, 2.0), (x: Double, y: Double) => x <= y))
  }
}
