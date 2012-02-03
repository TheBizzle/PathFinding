package datastructure.heap.test

import org.scalatest.{GivenWhenThen, FlatSpec}
import datastructure.heap.Heap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 6:57 PM
 */

class HeapSpec extends FlatSpec with GivenWhenThen {

    val heap = new Heap(( (a: Int, b: Int) => if (a < b) 1 else if (a == b) 0 else -1 ))

    behavior of "A Heap"

    it should "initialize, peek, and report its size correctly" in {

        given("a Heap that has been initialized without putting anything in it")
        when("examined")

        then("it should have a size of \"zero\"")
        heap.size === 0

        and("know that it's empty")
        heap.isEmpty === true

        and("its head element should be None")
        heap.peek === None

    }

    it should "insert and report its size correctly (some more)" in {

        given("a Heap that still has nothing in it")
        when("something is inserted")
        heap.insert(4)

        then("then it should report its new head properly")
        heap.peek.get === 4

        and("its size")
        heap.size === 1

        and("its emptiness")
        heap.isEmpty === false

    }

    it should "remove correctly" in {

        given("a Heap that has 4 elements in it")
        heap.insert(8)
        heap.insert(0)
        heap.insert(1)

        when("each element is removed (one at a time)")
        then("it should return the first one correctly")
        heap.remove() === 0

        and("reflect its size accordingly")
        heap.size === 3

        and("the second one")
        heap.remove() === 1

        and("reflect its size accordingly")
        heap.size === 2

        and("the third")
        heap.remove() === 4

        and("reflect its size accordingly")
        heap.size === 1

        and("the fourth")
        heap.remove() === 8

        and("reflect its size accordingly")
        heap.size === 0

        and("trying to remove the fifth, sixth, and seventh should throw a NoSuchElementException")
        intercept[NoSuchElementException] { heap.remove() }
        intercept[NoSuchElementException] { heap.remove() }
        intercept[NoSuchElementException] { heap.remove() }

        and("peeking at the fifth, sixth, and seventh should repeatedly result in \"None\"")
        heap.peek === None

        and("reflect its size accordingly")
        heap.size === 0

    }

    it should ("clone correctly") in {

        given("an empty Heap")
        when("cloned")
        val tempHeap = heap.clone()

        then("it should return an empty heap")
        tempHeap.size === 0
        tempHeap.peek === None

        given("a filled Heap")
        heap.insert(5)
        heap.insert(1)
        heap.insert(2)

        when("cloned")
        val anotherHeap = heap.clone()

        then("it should return an exact replica heap")
        anotherHeap.size === 3
        anotherHeap.remove() === 1
        anotherHeap.remove() === 2
        anotherHeap.remove() === 5

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

            heap.foreach { case x =>
                if ((x % 2) == 0) evenCount += 1
                else oddCount += 1
            }

            (evenCount, oddCount)

        }

        then("it should reply with the correct number of evens")
        evens === 3

        and("the correct number of odds")
        odds === 2

    }

}
