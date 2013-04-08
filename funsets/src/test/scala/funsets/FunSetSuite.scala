package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1 should contain 1")
      assert(contains(s2, 2), "Singleton 2 should contain 2")
      assert(!contains(s2, 1), "Singleton 2 should not contain 1")
    }
  }

  test("union contains all elements of both sets") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union should contain 1")
      assert(contains(s, 2), "Union should contain 2")
      assert(!contains(s, 3), "Union should not contain 3")
    }
  }

  test("intersect contains elements that are in both sets") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(singletonSet(1), singletonSet(3))
      val s = intersect(union1, union2)
      assert(contains(s, 1), "Intersect should contain 1")
      assert(!contains(s, 2), "Intersect should not contain 2")
      assert(!contains(s, 3), "Intersect should not contain 3")
    }
  }

  test("diff contains elements that are in the first set and not in the second set") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = singletonSet(1)
      val s = diff(union1, union2)
      assert(contains(s, 2), "Diff should contain 2")
    }
  }

  test("filter contains elements of the the first set that match a predicate") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(union1, singletonSet(3))
      val s = filter(union2, x => x > 1)
      assert(contains(s, 2), "Filter should contain 2")
      assert(contains(s, 3), "Filter should contain 3")
    }
  }

  test("forall returns false if any element doesn't match a predicate") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(union1, singletonSet(3))
      val s = forall(union2, x => x < 2)
      assert(!s, "Forall should return false to (x < 2)")
    }
  }

  test("forall returns true if all elements match a predicate") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(union1, singletonSet(3))
      val s = forall(union2, x => x > 0)
      assert(s, "Forall should return true to (x > 0)")
    }
  }

  test("exists returns false if all elements don't match a predicate") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(union1, singletonSet(3))
      val s = exists(union2, x => x < 0)
      assert(!s, "Forall should return false to (x < 0)")
    }
  }

  test("exists returns true if at least one element matches a predicate") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(union1, singletonSet(3))
      val s = exists(union2, x => x > 2)
      assert(s, "Forall should return true to (x > 2)")
    }
  }

  test("map returns a subset of all element transformed by f") {
    new TestSets {
      val union1: FunSets.Set = union(singletonSet(1), singletonSet(2))
      val union2: FunSets.Set = union(union1, singletonSet(3))
      val s = map(union2, x => x * 2)
      assert(contains(s, 2), "map should contain 2 for element 1")
      assert(contains(s, 4), "map should contain 4 for element 2")
      assert(!contains(s, 0), "map should not return 0 for element 0")
      assert(!contains(s, 8), "map should not return 8 for element 4")
      assert(contains(s, 6), "map should contain 6 for element 3")
    }
  }
}
