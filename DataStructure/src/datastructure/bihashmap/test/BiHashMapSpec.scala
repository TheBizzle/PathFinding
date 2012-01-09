package datastructure.bihashmap.test

import org.scalatest.{GivenWhenThen, FlatSpec}
import datastructure.bihashmap.BiHashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/8/12
 * Time: 4:43 PM
 */

class BiHashMapSpec extends FlatSpec with GivenWhenThen {

    val aList = List(5, 17, 1, 9, 4)
    val bList = List("five", "seventeen", "one", "nine", "four")
    val inList = aList.zip(bList)

    val biHash = BiHashMap(inList: _*)

    behavior of "A BiHashMap"

    it should "initialize correctly" in {

        given("a pre-initialized BHM")
        when("asked for the size and its parts")
        val size = biHash.size
        val aSet = biHash.ASet
        val bSet = biHash.BSet

        then("the size should be equal to the size of what was passed in")
        size === inList.size

        and("the set of As should hold all and only the As that were passed in")
        ((aSet.foldLeft(true)( (acc, x) => aList.contains(x) && acc )) && (aList.foldLeft(true)( (acc, x) => aSet.contains(x) && acc ))) === true

        and("the set of Bs should hold all and only the Bs that were passed in")
        ((bSet.foldLeft(true)( (acc, x) => bList.contains(x) && acc )) && (bList.foldLeft(true)( (acc, x) => bSet.contains(x) && acc ))) === true

    }

    it should "insert, retrieve, and assess equality correctly" in {

        given("an empty BHM")
        val bhm = BiHashMap[Int, String]()

        when("inserting the same elements inserted into it as were used to construct the original BHM")
        bhm.put(1, "one")
        bhm.put(9, "nine")
        bhm.put("four", 4)
        bhm.put("seventeen", 17)
        bhm.put(5, "five")

        then("it should retrieve existing elements correctly")
        ((bhm.get(1).get == "one") && (bhm.get("nine").get == 9) && bhm.get("four").get == 4) === true

        and("it should fail out on retrieving bad elements")
        ((bhm.get(2) == None) && (bhm.get("three") == None) && (bhm.get(6) == None)) === true

        and("it should have equality to the pre-made BHM")
        (bhm == biHash) === true

    }

    it should "clone and assess containment correctly" in {

        given("the pre-existing BHM")
        when("cloning, and comparing the two BHMs")
        val bhm = biHash.clone()

        then("the original should equal the clone")
        biHash === bhm

        and("the clone should equal the original")
        bhm === biHash

        val cloneAs = bhm.ASet
        val cloneBs = bhm.BSet
        val origAs = biHash.ASet
        val origBs = biHash.BSet

        and("the clone should contain all of the original's As")
        (origAs.foldLeft(true)( (acc, x) => cloneAs.contains(x) && acc )) === true

        and("the original should contain all of the clone's As")
        (cloneAs.foldLeft(true)( (acc, x) => origAs.contains(x) && acc )) === true

        and("the clone should contain all of the original's Bs")
        (origBs.foldLeft(true)( (acc, x) => cloneBs.contains(x) && acc )) === true

        and("the original should contain all of the clone's Bs")
        (cloneBs.foldLeft(true)( (acc, x) => origBs.contains(x) && acc )) === true

    }

    it should ("apply, remove, update, and clear correctly") in {

        given("a clone of the original BHM")
        val bhm = biHash.clone()

        when("manipulating the clone")
        then("it should apply correctly")
        bhm(1) === "one"
        bhm("five") == 5

        and("update As correctly")
        bhm.update(1, "eins")
        bhm("eins") === 1
        bhm(1) === "eins"

        and("update Bs correctly")
        bhm.update("five", 15)
        bhm(15) === "five"
        bhm("five") === 15

        and("remove correctly")
        bhm.remove("five")
        bhm.get("five") === None
        bhm.get(15) === None

        bhm.remove(1)
        bhm.get(1) === None
        bhm.get("one") === None

        and("apply on bads correctly")
        intercept[NoSuchElementException] {
            bhm("five")
        }
        intercept[NoSuchElementException] {
            bhm(15)
        }
        intercept[NoSuchElementException] {
            bhm(9001)
        }

        and("the clone should clear correctly")
        bhm.clear()
        bhm.size === 0
        bhm.ASet.size === 0
        bhm.BSet.size === 0
        bhm.get("five") === None
        bhm.get(17) === None
        bhm.get(3) === None

    }

    it should "be mutable correctly" in {

        given("an empty BHM")
        var bhm = BiHashMap[Int, String]()

        when("using it mutably")

        then("+= should work correctly")
        bhm += 8080 -> "eighty-eighty"
        bhm(8080) === "eighty-eighty"
        bhm("eighty-eighty") === 8080

        bhm += "zero" -> 0
        bhm(0) === "zero"
        bhm("zero") === 0

        and("-= should work correctly")
        bhm -= 8080
        bhm.get(8080) === None
        bhm.get("eighty-eighty") === None

        bhm -= "zero"
        bhm.get(0) === None
        bhm.get("zero") === None

    }

}