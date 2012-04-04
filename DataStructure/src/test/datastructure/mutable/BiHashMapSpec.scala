//package datastructure.mutable
//
//import org.scalatest.{GivenWhenThen, FlatSpec}
//import org.scalatest.matchers.ShouldMatchers
//
///**
// * Created by IntelliJ IDEA.
// * User: Jason
// * Date: 3/16/12
// * Time: 12:19 AM
// */
//
//class BiHashMapSpec extends FlatSpec with GivenWhenThen with ShouldMatchers {
//
//  val aList = List(5, 17, 1, 9, 4)
//  val bList = List("five", "seventeen", "one", "nine", "four")
//  val inList = aList.zip(bList)
//
//  val biHash = BiHashMap(inList: _*)
//
//  behavior of "A BiHashMap"
//
//  it should "initialize correctly" in {
//
//    given("a pre-initialized BHM")
//    when("asked for the size and its parts")
//    val size = biHash.size
//    val aSet = biHash.aValues
//    val bSet = biHash.aValues
//
//    then("the size should equal equal to the size of what was passed in")
//    size should equal (inList.size)
//
//    and("the set of As should hold all and only the As that were passed in")
//    aList.sorted.zipAll(aSet.toList.sorted, -1, "-1") map { case (x,y) => x should equal (y) }
//
//    and("the set of Bs should hold all and only the Bs that were passed in")
//    bList.sorted.zipAll(bSet.toList.sorted, -1, "-1") map { case (x,y) => x should equal (y) }
//
//  }
//
//}
