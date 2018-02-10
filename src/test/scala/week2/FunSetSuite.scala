package week2

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s4 = (x :Int) => 0 < x  & x <= 10
    val s5 = (x :Int) => 5 < x  & x <= 15
    val s6 = (x :Int) => 0 < x  & x <= 20
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
      assert(!contains(s1, 3), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains no elements of each set") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains elements of first set") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter contains only even numbers") {
    new TestSets {
      val s = filter(s4, x => x % 2 == 0)
      assert(contains(s, 2), "Filter 2")
      assert(contains(s, 4), "Filter 4")
      assert(contains(s, 10), "Filter 10")
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 5), "Filter 5")
      assert(!contains(s, 9), "Filter 9")
    }
  }

  test("forall") {
    val s = (x: Int) => 0 < x & x <= 10

    assert(forall(s, x => x > 0), "Forall s4 - x > 0")
    assert(!forall(s, x => x > 1), "Forall s4 - x > 1")
  }

  test("exists") {
    val s = (x :Int) => 0 < x  & x <= 50

    assert(exists(s, x => x > 0), "Exists (0, 50] - x > 0")
    assert(exists(s, x => x >= 50), "Exists (0, 50] - x >= 50")
    assert(!exists(s, x => x > 100), "Exists (0, 50] - x > 100")
  }

  test("map elements of set by adding 1") {
    val s = (x :Int) => 0 < x  & x <= 10
    val r = map(s, x => x + 1)

    assert(!contains(r, 1), "Map 1")
    assert(contains(r, 11), "Map 11")
  }

}
