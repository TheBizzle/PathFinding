package datastructure.heap

import org.scalatest.{BeforeAndAfterEach, GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 6:57 PM
 */

class HeapSpec extends FlatSpec with GivenWhenThen with BeforeAndAfterEach with ShouldMatchers {

  val heap = new Heap(( (a: Int, b: Int) => if (a < b) 1 else if (a == b) 0 else -1 ))

  override def beforeEach() {
    heap.clear()
    super.beforeEach()
  }

  behavior of "A Heap"

  it should "initialize, peek, and report its size correctly" in {

    given("a Heap that has been initialized without putting anything in it")
    when("examined")

    then("""it should have a size of "zero"""")
    heap.size should equal (0)

    and("know that it's empty")
    heap.isEmpty should equal (true)

    and("its head element should be None")
    heap.peek should equal (None)

  }

  it should "insert and report its size correctly (some more)" in {

    given("a Heap that still has nothing in it")
    when("something is inserted")
    heap.insert(4)

    then("then it should report its new head properly")
    heap.peek.get should equal (4)

    and("its size")
    heap.size should equal (1)

    and("its emptiness")
    heap.isEmpty should equal (false)

  }

  it should "remove correctly" in {

    given("a Heap that has 4 elements in it")
    heap.insert(4)
    heap.insert(8)
    heap.insert(0)
    heap.insert(1)

    when("each element is removed (one at a time)")
    then("it should return the first one correctly")
    heap.remove() should equal (0)

    and("reflect its size accordingly")
    heap.size should equal (3)

    and("the second one")
    heap.remove() should equal (1)

    and("reflect its size accordingly")
    heap.size should equal (2)

    and("the third")
    heap.remove() should equal (4)

    and("reflect its size accordingly")
    heap.size should equal (1)

    and("the fourth")
    heap.remove() should equal (8)

    and("reflect its size accordingly")
    heap.size should equal (0)

    and("trying to remove the fifth, sixth, and seventh should throw a NoSuchElementException")
    intercept[NoSuchElementException] { heap.remove() }
    intercept[NoSuchElementException] { heap.remove() }
    intercept[NoSuchElementException] { heap.remove() }

    and("peeking at the fifth, sixth, and seventh should repeatedly result in \"None\"")
    heap.peek should equal (None)

    and("reflect its size accordingly")
    heap.size should equal (0)

  }

  it should ("handle heterogenous adds and removes seamlessly") in {

    given("a pre-initialized Heap")
    heap.insert(7)
    heap.insert(9)
    heap.insert(3)
    heap.insert(4)
    heap.insert(1)
    heap.insert(5)

    when("poppped")
    then("it should return the proper value")
    heap.remove() should equal (1)

    when("a value is added to the end and a 'pop' occurs")
    then("it should return the proper value")
    heap.insert(13)
    heap.remove() should equal (3)

    when ("a value is added to the middle and a 'pop' occurs")
    then("it should return the proper value")
    heap.insert(6)
    heap.remove() should equal (4)

    when("a value is added to the front and a 'pop' occurs")
    then("it should return the newly-added value")
    heap.insert(1)
    heap.remove() should equal (1)

    when("another value is added to the middle and a 'pop' occurs")
    then("it should return the proper value")
    heap.insert(8)
    heap.remove() should equal (5)

    when("another value is added to the end and a 'pop' occurs")
    then("it should return the proper value")
    heap.insert(16)
    heap.remove() should equal (6)

    when("another value is added to the end and a 'pop' occurs")
    then("it should return the proper value")
    heap.insert(19)
    heap.remove() should equal (7)

    when("two values are added to the front and two 'pop's occur")
    then("it should return both of those values")
    heap.insert(4)
    heap.insert(6)
    heap.remove() should equal (4)
    heap.remove() should equal (6)

  }

  it should ("handle bursts of 'pop's correctly") in {

    given("a preinitialized Heap")
    heap.insert(68)
    heap.insert(53)
    heap.insert(9)
    heap.insert(88)
    heap.insert(13)
    heap.insert(43)
    heap.insert(36)
    heap.insert(4)
    heap.insert(21)
    heap.insert(58)
    heap.insert(27)
    heap.insert(97)
    heap.insert(84)
    heap.insert(71)
    heap.insert(78)
    heap.insert(50)
    heap.insert(62)
    heap.insert(91)
    heap.insert(32)
    heap.insert(46)
    heap.insert(51)
    heap.insert(94)
    heap.insert(5)
    heap.insert(45)

    when("poppped")
    then("it should return the proper value")
    heap.remove() should equal (4)

    when("a value is added and a 'pop' occurs")
    then("it should return the proper value")
    heap.insert(39)
    heap.remove() should equal (5)

    when("a burst of pops occur")
    then("they should all return the proper values")
    heap.remove() should equal (9)
    heap.remove() should equal (13)
    heap.remove() should equal (21)
    heap.remove() should equal (27)
    heap.remove() should equal (32)

    when("a value is added to the front and a 'pop occurs")
    then("it should return that value")
    heap.insert(3)
    heap.remove() should equal (3)

    when("another burst of pops occur")
    then("they should all return the proper values")
    heap.remove() should equal (36)
    heap.remove() should equal (39)
    heap.remove() should equal (43)
    heap.remove() should equal (45)

    when("a value is added to the end and a 'pop occurs")
    then("it should return the proper value")
    heap.insert(103)
    heap.remove() should equal (46)

    when("another burst of pops occur")
    then("they should all return the proper values")
    heap.remove() should equal (50)
    heap.remove() should equal (51)
    heap.remove() should equal (53)
    heap.remove() should equal (58)
    heap.remove() should equal (62)
    heap.remove() should equal (68)

    when("a value is added to the middle and a 'pop occurs")
    then("it should return the proper value")
    heap.insert(89)
    heap.remove() should equal (71)

    when("a final burst of pops occur")
    then("they should all return the proper values")
    heap.remove() should equal (78)
    heap.remove() should equal (84)
    heap.remove() should equal (88)

  }

  it should ("clone correctly") in {

    given("an empty Heap")
    when("cloned")
    val tempHeap = heap.clone()

    then("it should return an empty heap")
    tempHeap.size should equal (0)
    tempHeap.peek should equal (None)

    given("a filled Heap")
    heap.insert(5)
    heap.insert(1)
    heap.insert(2)

    when("cloned")
    val anotherHeap = heap.clone()

    then("it should return an exact replica heap")
    anotherHeap.size should equal (3)
    anotherHeap.remove() should equal (1)
    anotherHeap.remove() should equal (2)
    anotherHeap.remove() should equal (5)

  }

  it should ("know how to do operations on each element") in {

    given("a filled Heap")
    heap.insert(10)
    heap.insert(4)
    heap.insert(2)
    heap.insert(7)
    heap.insert(3)

    when("foreach()ed about the even/odd arity of its content")
    val (evens, odds) = {
      var evenCount = 0
      var oddCount = 0
      heap foreach ( x => if ((x % 2) == 0) evenCount += 1 else oddCount += 1)
      (evenCount, oddCount)
    }

    then("it should reply with the correct number of evens")
    evens should equal (3)

    and("the correct number of odds")
    odds should equal (2)

  }

}
