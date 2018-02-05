package week1

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import RecursionFunctions.pascal

  test("pascal: col=0,row=2") {
    assert(pascal(0, 2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1, 2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1, 3) === 3)
  }

  test("pascal: col=2,row=2") {
    assert(pascal(2, 2) === 1)
  }

  test("pascal: col=2,row=4") {
    assert(pascal(2, 4) === 6)
  }

  test("pascal: col=2,row=5") {
    assert(pascal(2, 5) === 10)
  }

  test("pascal: col=5,row=10") {
    assert(pascal(5, 10) === 252)
  }

}
