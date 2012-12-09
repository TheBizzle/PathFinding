package datastructure.mutable

import collection.GenTraversableOnce
import collection.mutable.ListBuffer

import org.scalatest.{ BeforeAndAfterEach, FunSuite, matchers }, matchers.ShouldMatchers

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:20 PM
 */

// The `sorted` method gets called a lot throughout here, since I want to keep tests general and flexible, but a `HashMap`'s ordering is largely unpredictable
class BiHashMapFunSuite extends FunSuite with BeforeAndAfterEach with ShouldMatchers {

  type A = Int
  type B = String
  type BHM = BiHashMap[A, B]

  type AB = (A, B)
  type BA = (B, A)

  val aList: List[A] = List(5, 17, 1, 9, 4)
  val bList: List[B] = List("five", "seventeen", "one", "nine", "four")
  val baseList: List[AB] = aList zip bList
  val biHash = BiHashMap[A, B]()

  override def beforeEach() {
    super.beforeEach()
    biHash.clear()
    biHash ++= baseList
  }

  test("==") {
    (biHash == BiHashMap[Double, B]()) should equal (false)
    (biHash == BiHashMap((aList tail) zip (bList tail) map { case (i, s) => (i.toDouble, s) }: _*)) should equal (false)
    (biHash == BiHashMap()) should equal (false)
    (biHash == BiHashMap((aList tail) zip (bList tail): _*)) should equal (false)
    (biHash == BiHashMap(baseList: _*)) should equal (true)
  }

  test("!=") {
    (biHash != BiHashMap[Double, B]()) should equal (true)
    (biHash != BiHashMap((aList tail) zip (bList tail) map { case (i, s) => (i.toDouble, s) }: _*)) should equal (true)
    (biHash != BiHashMap[A, B]()) should equal (true)
    (biHash != BiHashMap((aList tail) zip (bList tail): _*)) should equal (true)
    (biHash != BiHashMap(baseList: _*)) should equal (false)
  }

  test("+(elem)") {
    def forwards(bhm: BHM, target: BHM, elems: AB*) {
      testSameElems(bhm + elems(0),            target, false)
      testSameElems(bhm + elems(1),            target, false)
      testSameElems(bhm + elems(0) + elems(1), target, true)
    }
    def backwards(bhm: BHM, target: BHM, elems: BA*) {
      testSameElems(bhm + elems(0),            target, false)
      testSameElems(bhm + elems(1),            target, false)
      testSameElems(bhm + elems(0) + elems(1), target, true)
    }
    val myElems = Seq(9001 -> "nein tousend won", 4 -> "fier")
    forwards (biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  test("+(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: AB*) {
      testSameElems(bhm + (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
    }
    def backwards(bhm: BHM, target: BHM, elems: BA*) {
      testSameElems(bhm + (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
    }
    val myElems = Seq(9001 -> "nein tousend won", 4 -> "fier", 3 -> "shree, akchurry")
    forwards (biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  test("++(genTraversableOnce[AB])") {
    def forwards(bhm: BHM, target: GenTraversableOnce[AB], appendee: GenTraversableOnce[AB]) {
      testSameElems(bhm ++ bhm toList,      bhm toList)
      testSameElems(bhm ++ appendee toList, target toList)
    }
    def backwards(bhm: BHM, target: GenTraversableOnce[AB], appendee: GenTraversableOnce[BA]) {
      testSameElems(bhm ++ bhm.flip toList, bhm toList)
      testSameElems(bhm ++ appendee toList, target toList)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards (biHash.clone, List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone, List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  test("++:(traversableOnce[AB])") {
    def forwards(bhm: BHM, target: Traversable[AB], appendee: Traversable[AB]) {
      testSameElems(bhm ++: bhm toList,      bhm toList)
      testSameElems(bhm ++: appendee toList, target toList)
    }
    def backwards(bhm: BHM, target: Traversable[AB], appendee: Traversable[BA]) {
      testSameElems(bhm.flip ++: bhm.flip map (_.swap) toList, bhm toList)
      testSameElems(bhm.flip ++: appendee map (_.swap) toList, target toList)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards (biHash.clone, List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone, List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  test("++=(traversableOnce[AB])") {

    val original = biHash.clone
    original ++= original
    testSameElems(original, biHash)

    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    val target = List((biHash.toSeq ++ myList): _*)

    val subjectF = biHash.clone
    testSameElems(subjectF ++= myList, target)
    testSameElems(subjectF, target)

    val subjectB = biHash.clone
    testSameElems(subjectB ++= (myList map (_.swap)), target)
    testSameElems(subjectB, target)

  }

  test("+=(elem)") {

    val myElem = 9001 -> "was?! mein thousand?!!?!?!?!"
    val target = BiHashMap(myElem :: baseList: _*)

    val subjectF = biHash.clone
    testSameElems(subjectF += myElem, target)
    testSameElems(subjectF, target)

    val subjectB = biHash.clone
    testSameElems(subjectB += myElem.swap, target)
    testSameElems(subjectB, target)

  }

  test("+=(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: AB*) {
      testSameElems(bhm += (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
      testSameElems(bhm, target)
    }
    def backwards(bhm: BHM, target: BHM, elems: BA*) {
      testSameElems(bhm += (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
      testSameElems(bhm, target)
    }
    val myElems = List(9001 -> "was?! mein thousand?!!?!?!?!", 9002 -> "ja, dein thousand!", 91124 -> "no wai!", 90210 -> "yahweh!")
    forwards (biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  test("-(elem)") {
    def forwards(bhm: BHM, targetElemPairs: (BHM, A)*) {
      targetElemPairs.foldLeft(bhm){ case (acc, (target, elem)) => val x = acc - elem; testSameElems(x, target); x }
    }
    def backwards(bhm: BHM, targetElemPairs: (BHM, B)*) {
      targetElemPairs.foldLeft(bhm){ case (acc, (target, elem)) => val x = acc - elem; testSameElems(x, target); x }
    }
    val myPairs = baseList.tails.toSeq drop 1 map (BiHashMap[A, B](_: _*)) zip baseList
    forwards (biHash.clone, myPairs map { case (target, elem) => (target, elem._1) }: _*)
    backwards(biHash.clone, myPairs map { case (target, elem) => (target, elem._2) }: _*)
  }

  test("-(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: A*) {
      testSameElems(bhm - (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
    }
    def backwards(bhm: BHM, target: BHM, elems: B*) {
      testSameElems(bhm - (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
    }
    val (removables, pretarget) = baseList splitAt (baseList.size - 1)
    forwards (biHash.clone, BiHashMap(pretarget: _*), removables map (_._1): _*)
    backwards(biHash.clone, BiHashMap(pretarget: _*), removables map (_._2): _*)
  }

  test("--(that)") {
    def forwards(bhm: BHM, target: TraversableOnce[AB], removees: TraversableOnce[A]) {
      (bhm -- bhm.aValues) should equal (BiHashMap.empty)
      testSameElems(bhm -- removees, target)
    }
    def backwards(bhm: BHM, target: TraversableOnce[AB], removees: TraversableOnce[B]) {
      (bhm -- bhm.bValues) should equal (BiHashMap.empty)
      testSameElems(bhm -- removees, target)
    }
    val myList = baseList dropRight 2
    forwards (biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._1))
    backwards(biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._2))
  }

  test("--=(traversableOnce[A])") {
    def forwards(bhm: BHM, target: TraversableOnce[AB], removees: TraversableOnce[A]) {

      val subjectEmpty = bhm.clone
      (subjectEmpty --= bhm.aValues) should equal (BiHashMap.empty)
      subjectEmpty should equal (BiHashMap.empty)

      val subject = bhm.clone
      testSameElems(subject --= removees, target)
      testSameElems(subject, target)

    }
    def backwards(bhm: BHM, target: TraversableOnce[AB], removees: TraversableOnce[B]) {

      val subjectEmpty = bhm.clone
      (subjectEmpty --= bhm.bValues) should equal (BiHashMap.empty)
      subjectEmpty should equal (BiHashMap.empty)

      val subject = bhm.clone
      testSameElems(subject --= removees, target)
      testSameElems(subject, target)

    }
    val myList = baseList dropRight 2
    forwards (biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._1))
    backwards(biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._2))
  }

  test("-=(elem)") {
    def forwards(bhm: BHM, target: BHM, elem: AB) {
      testSameElems(bhm -= elem._1, target)
      testSameElems(bhm, target)
    }
    def backwards(bhm: BHM, target: BHM, elem: BA) {
      testSameElems(bhm -= elem._1, target)
      testSameElems(bhm, target)
    }
    val myElem = baseList(1)
    val pretarget = baseList filterNot(_ == myElem)
    forwards (biHash.clone, BiHashMap(pretarget: _*), myElem)
    backwards(biHash.clone, BiHashMap(pretarget: _*), myElem.swap)
  }

  test("-=(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: A*) {
      testSameElems(bhm -= (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
      testSameElems(bhm, target)
    }
    def backwards(bhm: BHM, target: BHM, elems: B*) {
      testSameElems(bhm -= (elems(0), elems(1), elems.splitAt(2)._2: _*), target)
      testSameElems(bhm, target)
    }
    val (removables, pretarget) = baseList splitAt (baseList.size - 1)
    forwards (biHash.clone, BiHashMap(pretarget: _*), removables map (_._1): _*)
    backwards(biHash.clone, BiHashMap(pretarget: _*), removables map (_._2): _*)
  }

  test("->") {
    biHash -> 2 should equal ((biHash, 2))
  }

  // Cryptic symbol for "foldLeft"
  test("/:") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _))
    biHash.clone./:((1, ""))(f) should equal (expected)
  }

  // Cryptic symbol for "fold"
  test("""/:\"""") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduce (_ + _))
    biHash.clone./:\((1, ""))(f) should equal (expected)
  }

  // Cryptic symbol for "foldRight"
  test(""":\""") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _))
    biHash.clone.:\((1, ""))(f) should equal (expected)
  }

  test("aIterator") {
    val iter   = biHash.aIterator
    val target = baseList.map(_._1).toIterator
    iter.toSeq.sorted.sameElements(target.toSeq.sorted) should be (true)
  }

  test("addString(sb, sep, start, end)") {
    val (sep, start, end) = ("york", "dork", "bjork")
    val target1 = baseList.addString(new StringBuilder()).toString replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    val target2 = baseList.addString(new StringBuilder(), sep).toString replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    val target4 = baseList.addString(new StringBuilder(), sep, start, end).toString replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    biHash.addString(new StringBuilder()).toString.sorted should equal (target1.sorted)                    // One-arg version
    biHash.addString(new StringBuilder(), sep).toString.sorted should equal (target2.sorted)               // Two-arg version
    biHash.addString(new StringBuilder(), sep, start, end).toString.sorted should equal (target4.sorted)   // Four-arg version
  }

  test("aggregate(b)(seqop, combop)") {
    val expected = biHash map (_._1) product
    val seqF   = (acc: A, elem: AB) => acc * elem._1
    val comboF = (_: A, _: A) => -1
    biHash.clone.aggregate(1)(seqF, comboF) should equal (expected)
  }

  test("andThen(func)") {

    val bFunc: (B) => B = (b: B) => identity(b)
    biHash andThen bFunc apply baseList(0)._1 should equal (baseList(0)._2)

    val aFunc: (A) => A = (a: A) => identity(a)
    biHash andThen aFunc apply baseList(0)._2 should equal (baseList(0)._1)

  }

  test("apply(elem)") {
    biHash(baseList(0)._1) should equal (baseList(0)._2)
    biHash(baseList(1)._2) should equal (baseList(1)._1)
  }

  test("aSet") {
    val iter   = biHash.aSet
    val target = baseList.map(_._1).toSet
    iter.toSeq.sorted.sameElements(target.toSeq.sorted) should be (true)
  }

  test("bIterator") {
    val iter   = biHash.bIterator
    val target = baseList.map(_._2).toIterator
    iter.toSeq.sorted.sameElements(target.toSeq.sorted) should be (true)
  }

  test("bSet") {
    val iter   = biHash.bSet
    val target = baseList.map(_._2).toSet
    iter.toSeq.sorted.sameElements(target.toSeq.sorted) should be (true)
  }

  test("clear") {
    biHash.clear
    biHash should equal (BiHashMap[A, B]())
  }

  test("clone") {
    biHash.clone should equal (biHash)
  }

  test("collect(pf)") {
    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val aComparator = (a: A) => a < aAverage
    biHash.collect{ case (a: A, b: B) if (a == null) => b }          .toList.sorted should equal (List())                                                                          // Match (none)
    biHash.collect{ case (a: A, b: B) if (a == abList.head._1) => b }.toList.sorted should equal (List(abList.head._2).sorted)                                                     // Match (one)
    biHash.collect{ case (a: A, b: B) if (aComparator(a)) => b }     .toList.sorted should equal (List(abList.filter(entry => aComparator(entry._1)).toSeq map (_._2): _*) sorted) // Match (some)
  }

  test("collectFirst(pf)") {
    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val aComparator = (a: A) => a < aAverage
    biHash collectFirst { case (a: A, b: B) if (a == null) => b }           should equal (None)                                                        // Match (none)
    biHash collectFirst { case (a: A, b: B) if (a == abList.head._1) => b } should equal (Some(abList.head._2))                                        // Match (one)
    biHash collectFirst { case (a: A, b: B) if (aComparator(a)) => b }      should equal (Some(abList.filter(entry => aComparator(entry._1)).head._2)) // Match (some)
  }

  test("compose(func)") {

    val aFunc: (A) => A = (a: A) => a + 1
    intercept[NoSuchElementException] { biHash compose aFunc apply baseList.minBy(_._1)._1 }
    (biHash compose aFunc apply baseList.minBy(_._1)._1 - 1) should equal (baseList.minBy(_._1)._2)

    val bFunc: (B) => B = (b: B) => new B(b.getBytes map (x => (x ^ 2).toByte))
    intercept[NoSuchElementException] { biHash compose bFunc apply baseList.head._2 }
    biHash compose bFunc apply bFunc(baseList.head._2) should equal (baseList.head._1)

  }

  test("contains(elem)") {

    val badA = 3421
    val badB = "I'm bad!  Let's go eat some cheeseburgers with President Ronnie!"

    biHash.contains(badA) should not be (true)
    biHash.contains(badB) should not be (true)

    biHash.contains(baseList.head._1) should be (true)
    biHash.contains(baseList.head._2) should be (true)

  }

  test("copyToArray(arr)") {

    val abArr = new Array[AB](biHash.size)
    biHash.copyToArray[AB](abArr)
    abArr.sorted should equal (biHash.toArray.sorted)

    val baArr = new Array[BA](biHash.size)
    biHash.copyToArray[BA](baArr)
    baArr.sorted should equal (biHash.flip.toArray.sorted)

  }

  test("copyToArray(arr, start)") {

    def mockResult[T, U](bhm: BiHashMap[T, U], start: Int) : Array[(T, U)] = {
      val mock = new Array[(T, U)](bhm.size)
      bhm.toArray.zipWithIndex foreach {
        case (tup, i) =>
          val newI = i + start
          mock(newI % bhm.size) = if (i < (bhm.size - start)) tup else null
      }
      mock
    }

    val start = 2

    val abArr = new Array[AB](biHash.size)
    biHash.copyToArray[AB](abArr, start)
    abArr should equal (mockResult(biHash, start))

    val baArr = new Array[BA](biHash.size)
    biHash.copyToArray[BA](baArr, start)
    baArr should equal (mockResult(biHash.flip, start))

  }

  test("copyToArray(arr, start, len)") {

    def mockResult[T, U](bhm: BiHashMap[T, U], start: Int, len: Int) : Array[(T, U)] = {
      val mock = new Array[(T, U)](bhm.size)
      bhm.toArray.zipWithIndex foreach {
        case (tup, i) =>
          val newI = i + start
          mock(newI % bhm.size) = if (i < len) tup else null
      }
      mock
    }

    val start = 2
    val len   = biHash.size - start - 1

    val abArr = new Array[AB](biHash.size)
    biHash.copyToArray[AB](abArr, start, len)
    abArr should equal (mockResult(biHash, start, len))

    val baArr = new Array[BA](biHash.size)
    biHash.copyToArray[BA](baArr, start, len)
    baArr should equal (mockResult(biHash.flip, start, len))

  }

  test("copyToBuffer(buff)") {

    val abBuff = new ListBuffer[AB]()
    biHash.copyToBuffer[AB](abBuff) //@@ Note: Another place where this fails is when method type parameter annotations like `C >: AB` and `C >: BA` have to be used and it must decide which to pick
    abBuff.toList.sorted should equal (biHash.toList.sorted)

    val baBuff = new ListBuffer[BA]()
    biHash.copyToBuffer[BA](baBuff)
    baBuff.toList.sorted should equal (biHash.flip.toList.sorted)

  }

  test("count(func)") {
    val abFunc = (ab: AB, a: A) => ab._1 < a
    val abList = baseList
    abList map (_._1) foreach (x => biHash count (abFunc(_, x)) should equal (abList count (abFunc(_, x))))
  }

  test("default(elem)") {

    val aDef = 4879
    intercept [NoSuchElementException] { biHash.default(aDef) }

    val bDef = "alksdalsdk"
    intercept [NoSuchElementException] { biHash.default(bDef) }

  }

  test("drop(n)") {

    val cleansed = biHash drop 2
    cleansed.toList.distinct.size should equal (baseList.size - 2)
    cleansed.toList foreach (baseList should contain (_))

    val allCleansed = biHash drop (baseList.size + 10)
    allCleansed.size should equal (0)

  }

  test("dropRight(n)") {

    val cleansed = biHash dropRight 2
    cleansed.toList.distinct.size should equal (baseList.size - 2)
    cleansed.toList foreach (baseList should contain (_))

    val allCleansed = biHash dropRight (baseList.size + 10)
    allCleansed.size should equal (0)

  }

  test("dropWhile(func)") {

    val abCleansed = biHash dropWhile (_._1 <= (baseList map (_._1) max))
    abCleansed.size should equal (0)

    var counter = 0
    val cleansed = biHash dropWhile { case _ => counter += 1; counter < baseList.size }
    cleansed.size should equal (1)

  }

  test("empty") {
    biHash.empty should equal (BiHashMap[A, B]())
  }

  test("ensuring(cond)") {
    intercept[AssertionError] { biHash ensuring ({false}) }
    biHash ensuring (true) should equal (biHash)
  }

  test("ensuring(bln)") {
    intercept[AssertionError] { biHash ensuring (false) }
    biHash ensuring (true) should equal (biHash)
  }

  test("equals(any)") {
    val elem = 1001 -> "herpy derpy"
    biHash should equal (biHash)
    biHash should not equal (biHash.clone += elem)
    (biHash.clone += elem) should equal (biHash.clone += elem)
  }

  test("exists(func)") {
    biHash.exists(_._1 == 3421) should equal (false)
    biHash.exists(_._1 == baseList.head._1) should equal (true)
  }

  test("filter(func)") {
    testSameElems(biHash filter (_._1 != 3421),             biHash)
    testSameElems(biHash filter (_._1 == baseList.head._1), BiHashMap(baseList.head))
  }

  test("filterAs(func)") {
    testSameElems(biHash filterAs (a => baseList map (_._2) contains (a)), biHash.empty)
    testSameElems(biHash filterAs (a => baseList map (_._1) contains (a)), biHash)
    testSameElems(biHash filterAs (_ == baseList.head._1),                 BiHashMap(baseList.head))
    testSameElems(biHash filterAs (_ != baseList.tail.head._1),            BiHashMap((baseList.head :: baseList.tail.tail): _*))
  }

  test("filterBs(func)") {
    testSameElems(biHash filterBs (b => baseList map (_._1) contains (b)), biHash.empty)
    testSameElems(biHash filterBs (b => baseList map (_._2) contains (b)), biHash)
    testSameElems(biHash filterBs (_ == baseList.head._2),                 BiHashMap(baseList.head))
    testSameElems(biHash filterBs (_ != baseList.tail.head._2),            BiHashMap((baseList.head :: baseList.tail.tail): _*))
  }

  test("filterNot(func)") {
    testSameElems(biHash filterNot (_._1 == 3421),             biHash)
    testSameElems(biHash filterNot (_._1 != baseList.head._1), BiHashMap(baseList.head))
  }

  test("find(func)") {
    biHash find (_._1 == 3421)             should equal (None)
    biHash find (_._1 == baseList.head._1) should equal (Some(baseList.head))
  }

  test("flatMap(func)") {
    val f = (entry: AB) => (entry._1 * 6, entry._2)
    val g = (entry: AB) => BiHashMap(f(entry))
    (biHash flatMap g) should equal (BiHashMap((baseList map f): _*))
  }

  test("flip") {
    biHash.flip should equal (BiHashMap(baseList map (_.swap): _*))
  }

  test("fold(res)(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduce (_ + _))
    biHash.clone.fold((1, ""))(f) should equal (expected)
  }

  test("foldLeft(res)(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _))
    biHash.clone.foldLeft((1, ""))(f) should equal (expected)
  }

  test("foldRight(res)(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _))
    biHash.clone.foldRight((1, ""))(f) should equal (expected)
  }

  test("forall(func)") {
    biHash forall (baseList.contains(_))     should be (true)
    biHash forall (_._2 == "five")           should be (false)
    biHash forall (_._2 == "lkjhadskjhsadl") should be (false)
  }

  test("foreach(func)") {

    val accInit = (0, "")
    var acc     = accInit
    val aOp     = (a: A) => a * 2
    val aFunc   = (a: A) => acc._1 + aOp(a)
    val bOp     = (b: B) => b + "!"
    val bFunc   = (b: B) => acc._2 + bOp(b)
    val abFunc  = (ab: AB) => (aFunc(ab._1), bFunc(ab._2))

    biHash foreach { case (a: A, b: B) => acc = abFunc((a, b)) }
    acc = (acc._1, acc._2.sorted)
    acc should equal ((aList map aOp sum), (bList map bOp reduce (_ + _) sorted))

  }

  test("formatted(str)") {
    val formatStr = "Herp, %s, I'm a derp."
    biHash.formatted(formatStr) should equal (formatStr.format(biHash.toString))
  }

  test("get(elem)") {
    biHash.get(baseList(0)._1) should equal (Option(baseList(0)._2))
    biHash.get(baseList(1)._2) should equal (Option(baseList(1)._1))
  }

  test("getOrElse(a, default)") {

    val badA = 98384
    val badB = "MY EMPORER!  I'VE FAAAAAAILED YOOOOOOU!"

    biHash getOrElse (baseList(2)._1, badB) should equal (baseList(2)._2)
    biHash getOrElse (badA, badB) should equal (badB)

    biHash getOrElse (baseList(2)._2, badA) should equal (baseList(2)._1)
    biHash getOrElse (badB, badA) should equal (badA)

  }

  test("getOrElseUpdate(a, => b)") {

    val badA = 98384
    val badB = "MY EMPORER!  I'VE FAAAAAAILED YOOOOOOU!"

    biHash getOrElseUpdate (baseList(2)._1, badB) should equal (baseList(2)._2)
    biHash getOrElseUpdate (badA, badB)           should equal (badB)

    biHash getOrElseUpdate (baseList(2)._2, badA) should equal (baseList(2)._1)
    biHash getOrElseUpdate (badB, badA)           should equal (badA)

  }

  // Yeah... try making sense of _this_ test
  test("groupBy(f)") {
    val newKV = baseList.head._1 * 1000 -> baseList.head._2
    biHash += newKV
    biHash groupBy (_._2) should equal (Map(baseList.head._2 -> BiHashMap(baseList.head, newKV)) ++ (baseList.tail map (x => x._2 -> BiHashMap(x))))
  }

  test("grouped(int)") {
    intercept[IllegalArgumentException] { biHash grouped (0) }
    ((biHash grouped (1) toList) map (_.head)).sorted zip baseList.sorted foreach { case (x, y) => x should equal (y) }
  }

  test("hasDefiniteSize") {
    biHash.hasDefiniteSize should equal (true)
  }

  test("head") {
    baseList exists (_ == biHash.head) should equal (true)
  }

  test("headOption") {
    baseList exists (_ == biHash.headOption.get) should equal (true)
  }

  test("init") {
    biHash.init.size should equal (baseList.size - 1)
    biHash.init.toList.distinct.size should equal (baseList.size - 1)
  }

  test("inits") {
    def checkInits(bhm: BHM, inits: Iterator[BHM]) {
      inits.next should equal (bhm)
      if (bhm.nonEmpty) checkInits(bhm.init, inits)
    }
    checkInits(biHash, biHash.inits)
  }

  test("isDefinedAt(key)") {
    biHash.isDefinedAt(baseList(0)._1) should equal (true)
    biHash.isDefinedAt(baseList(0)._2) should equal (true)
    biHash.isDefinedAt(-1243) should equal (false)
    biHash.isDefinedAt("crapple-y apple-y") should equal (false)
  }

  test("isEmpty") {
    biHash should not be ('empty)
    BiHashMap[A, B]() should be ('empty)
  }

  test("isTraversableAgain") {
    biHash should be ('traversableAgain)
  }

  test("iterator") {
    testSameElems(biHash.iterator, baseList.iterator)
  }

  test("last") {
    baseList exists (_ == biHash.last) should be (true)
  }

  test("lastOption") {
    baseList exists (_ == biHash.lastOption.get) should be (true)
  }

  test("map(func)") {
    val func = (entry: AB) => (entry._1 * 6, entry._2)
    (biHash map func) should equal (BiHashMap((baseList map func): _*))
  }

  test("mapAs") {
    val mapped   = biHash mapAs (_.toDouble)
    val knockoff = biHash map { case (a: A, b: B) => (a.toDouble, b) }
    mapped.toSeq.sorted.sameElements(knockoff.toSeq.sorted) should be (true)
    biHash.empty mapAs (_.toDouble) should be ('empty)
  }

  test("mapBs") {
    val mapped   = biHash mapBs (_ + "!")
    val knockoff = biHash map { case (a: A, b: B) => (a, b + "!") }
    mapped.toSeq.sorted.sameElements(knockoff.toSeq.sorted) should be (true)
    biHash.empty mapBs (_ + "!") should be ('empty)
  }

  test("mapResult") {
    val func = (entry: AB) => (entry._1, new B(entry._2.getBytes))
    testSameElems(biHash.mapResult(_ map func).result(), BiHashMap(baseList map func: _*))
  }

  test("maxBy") {
    biHash maxBy (_._1) should equal (baseList maxBy (_._1))
  }

  test("minBy") {
    biHash minBy (_._1) should equal (baseList.minBy(_._1))
  }

  // All three variants
  // I sort the strings because I can't guarantee that the list and the bidirectional map will be in the same order (they almost never are),
  // but it's good enough proof of correctness for me if the two strings are of the same length and contain all of the same letters
  test("mkString") {
    val (sep, start, end) = ("york", "dork", "bjork")
    def morph(str: String) = str replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    val target0 = morph(baseList.mkString).sorted
    val target1 = morph(baseList.mkString(sep)).sorted
    val target3 = morph(baseList.mkString(sep, start, end)).sorted
    biHash.mkString.sorted should equal (target0)                    // No-arg version
    biHash.mkString(sep).sorted should equal (target1)               // One-arg version
    biHash.mkString(sep, start, end).sorted should equal (target3)   // Three-arg version
  }

  test("nonEmpty") {
    biHash should be ('nonEmpty)
    BiHashMap[A, B]() should not be ('nonEmpty)
  }

  test("orElse(pFunc)") {

    val errorResB = "NOOOOOOO!"
    val orElseAB  = biHash.orElse[A, B]{ case _: A => errorResB }
    orElseAB(baseList.head._1) should equal (baseList.head._2)
    orElseAB(1543289513)       should equal (errorResB)

    val errorResA = 123123123
    val orElseBA = biHash.orElse[B, A]{ case _: B => errorResA }
    orElseBA(baseList.head._2)       should equal (baseList.head._1)
    orElseBA("alksjdfl;kasjflkasdj") should equal (errorResA)

  }

  test("partition(func)") {
    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val abComparator = (ab: AB) => (ab._1 < aAverage)
    biHash.partition(_ == null)              should equal ((biHash.empty, biHash))                                                                        // Match (none, all)
    biHash.partition(_._1 == abList.head._1) should equal ((BiHashMap(abList.head), BiHashMap(abList.tail: _*)))                                          // Match (one, rest)
    biHash.partition(abComparator)           should equal ((BiHashMap(abList filter (abComparator): _*), BiHashMap(abList filterNot (abComparator): _*))) // Match (some, others)
  }

  test("put(elem)") {
    val elem  = 9001 -> "was?! mein thousand?!!?!?!?!"
    val myMap = biHash.clone
    (myMap.put(elem._1, elem._2)) should equal (None)
    testSameElems(myMap, BiHashMap(elem :: baseList: _*))
    (myMap.put(elem._1, "derp")).get should equal (elem._2) //@@ Note: Part of why my hack is broken: `myMap.put _` is a compiler error, since it doesn't know which to choose; essentially, tupling and currying are useless with most of these methods
  }

  test("reduce(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduce (_ + _))
    biHash.clone reduce f should equal (expected)
  }

  test("reduceLeft(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _))
    biHash.clone reduceLeft f should equal (expected)
  }

  test("reduceLeftOption(func)") {

    def testFunc(bhm: BHM, target: Option[AB], func: (AB, AB) => AB) {
      (bhm reduceLeftOption func) should equal (target)
    }

    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expectedSome = Option((biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _)))

    testFunc(biHash.clone, expectedSome, f)
    testFunc(biHash.empty, None,         f)

  }

  test("reduceOption(func)") {

    def testFunc(bhm: BHM, target: Option[AB], func: (AB, AB) => AB) {
      (bhm reduceOption func) should equal (target)
    }

    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expectedSome = Option((biHash map (_._1) product, biHash map (_._2) reduce (_ + _)))

    testFunc(biHash.clone, expectedSome, f)
    testFunc(biHash.empty, None,         f)

  }

  test("reduceRight(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _))
    biHash.clone reduceRight f should equal (expected)
  }

  test("reduceRightOption(func)") {

    def testFunc(bhm: BHM, target: Option[AB], func: (AB, AB) => AB) {
      (bhm reduceRightOption func) should equal (target)
    }

    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expectedSome = Option((biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _)))

    testFunc(biHash.clone, expectedSome, f)
    testFunc(biHash.empty, None,         f)

  }

  test("remove(elem)") {
    biHash.remove(baseList(0)._1) should equal (Some(baseList(0)._2))
    biHash.remove(baseList(1)._2) should equal (Some(baseList(1)._1))
    biHash.remove(baseList(0)._1) should equal (None)
    biHash.remove(baseList(1)._2) should equal (None)
  }

  test("retain(func)") {

    val headMapAB = biHash.clone.retain { case (a, b) => (a, b) == biHash.head }
    testSameElems(headMapAB, BiHashMap(biHash.head))

    val tailMapAB = biHash.clone.retain { case (a, b) => biHash.tail.exists(_ == (a, b)) }
    testSameElems(tailMapAB, BiHashMap(biHash.tail.toSeq: _*))

    val allMapAB = biHash.clone.retain((_, _) => true)
    testSameElems(allMapAB, biHash)

  }

  test("sameElements(that)") {
    testSameElems(biHash,       baseList)
    testSameElems(biHash,       biHash.empty, false)
    testSameElems(biHash.empty, BiHashMap[A, B]())
    biHash.empty.sameElements(BiHashMap[B, A]()) should be (true)
  }

  test("scan(res)(func)") {
    val (abBase, abFunc) = ((0, ""), (x: AB, y: AB) => (x._1 + y._1, x._2 + y._2))
    testSameElems(biHash.scan(abBase)(abFunc), biHash.toList.scan(abBase)(abFunc))
  }

  test("scanLeft(res)(func)") {
    val (abBase, abFunc) = ((0, ""), (x: AB, y: AB) => (x._1 + y._1, x._2 + y._2))
    testSameElems(biHash.scanLeft(abBase)(abFunc), biHash.toList.scanLeft(abBase)(abFunc))
  }

  test("scanRight(res)(func)") {
    val (abBase, abFunc) = ((0, ""), (x: AB, y: AB) => (x._1 + y._1, x._2 + y._2))
    testSameElems(biHash.scanRight(abBase)(abFunc), biHash.toList.scanRight(abBase)(abFunc))
  }

  test("size") {
    BiHashMap[A, B]().size should equal (0)
    BiHashMap(baseList(0)).size should equal (1)
    biHash.size should equal (baseList.size)
  }

  test("slice(from, until)") {
    val slice = biHash.slice(1, 4)
    slice should have size (3)
    slice foreach (baseList.contains(_) should equal (true))
  }

  test("sliding(size)") {
    val slider = biHash.sliding(2)
    val window = slider.next()
    window should have size (2)
    window foreach (baseList.contains(_) should equal (true))
  }

  test("sliding(size, step)") {
    val slider = biHash.sliding(2, 2)
    val window = slider.next()
    window should have size (2)
    window foreach (baseList.contains(_) should equal (true))
  }

  test("span(func)") {

    val (preAB1, postAB1) = biHash.span(baseList.contains(_))
    postAB1 should have size (0)
    testSameElems(preAB1, baseList)

    val (preAB2, postAB2) = biHash.span(!baseList.contains(_))
    preAB2 should have size (0)
    testSameElems(postAB2, baseList)

    val (preAB3, postAB3) = biHash.span(_ == biHash.head)
    preAB3 should have size (1)
    preAB3.head should equal (biHash.head)
    postAB3 should have size (biHash.size - 1)

  }

  test("splitAt(n)") {
    val (before, after) = biHash.splitAt(baseList.length / 2)
    before forall (entry => after.get(entry._1) == None) should be (true)
    (before.size + after.size) should equal (baseList.size)
    before.size should equal (baseList.length / 2)
    after.size should equal (baseList.length - (baseList.length / 2))
  }

  test("swap") {
    biHash.swap should equal (BiHashMap(baseList map (_.swap): _*))
  }

  test("tail") {
    biHash.tail.size should equal (baseList.size - 1)
    biHash.tail.toList.distinct.size should equal (baseList.size - 1)
  }

  test("tails") {
    def checkTails(bhm: BHM, tails: Iterator[BHM]) {
      tails.next should equal (bhm)
      if (bhm.nonEmpty) checkTails(bhm.tail, tails)
    }
    checkTails(biHash, biHash.tails)
  }

  test("take(n)") {

    val taken = biHash take 2
    taken.toList.distinct.size should equal (2)
    taken.toList foreach (baseList should contain (_))

    val allTaken = biHash take (baseList.size + 10)
    allTaken.size should equal (baseList.size)
    allTaken.toList.distinct.size should equal (baseList.size)
    allTaken.toList foreach (baseList should contain (_))

  }

  test("takeRight(n)") {

    val taken = biHash takeRight 2
    taken.toList.distinct.size should equal (2)
    taken.toList foreach (baseList should contain (_))

    val allTaken = biHash takeRight (baseList.size + 10)
    allTaken.size should equal (baseList.size)
    allTaken.toList.distinct.size should equal (baseList.size)
    allTaken.toList foreach (baseList should contain (_))

  }

  test("takeWhile(func)") {

    val abTaken = biHash takeWhile (_._1 <= (baseList map (_._1) max))
    abTaken.toList.distinct.size should equal (baseList.size)
    abTaken.size should equal (baseList.size)

    var counter = 0
    val taken = biHash takeWhile { case _ => counter += 1; counter < baseList.size }
    taken.toList.distinct.size should equal (baseList.size - 1)
    taken.size should equal (baseList.size - 1)

  }

  test("toArray") {
    testSameElems(baseList.toArray, biHash.toArray)
  }

  test("toBuffer") {
    testSameElems(baseList.toBuffer, biHash.toBuffer)
  }

  test("toIndexedSeq") {
    testSameElems(baseList.toIndexedSeq, biHash.toIndexedSeq)
  }

  test("toIterable") {
    testSameElems(baseList.toIterable, biHash.toIterable)
  }

  test("toIterator") {
    testSameElems(baseList.toIterator, biHash.toIterator)
  }

  test("toList") {
    testSameElems(baseList.toList, biHash.toList)
  }

  test("toMap") {
    testSameElems(baseList.toMap, biHash.toMap)
  }

  test("toSeq") {
    testSameElems(baseList.toSeq, biHash.toSeq)
  }

  test("toSet") {
    testSameElems(baseList.toSet, biHash.toSet)
  }

  test("toStream") {
    testSameElems(baseList.toStream, biHash.toStream)
  }

  test("toTraversable") {
    testSameElems(baseList.toTraversable, biHash.toTraversable)
  }

  test("transform(func)") {
    val abFunc: (A, B) => B = (k: A, v: B) => v + "_" + k.toString
    val abTransformed = biHash transform abFunc
    abTransformed should equal (BiHashMap((baseList map { case (k, v) => (k, (abFunc(k, v))) }): _*))
  }

  test("unzip") {
    val (aColl1, bColl1) = biHash.unzip
    val (aColl2, bColl2) = baseList.unzip
    aColl2.toSeq.sorted.sameElements(aColl1.toSeq.sorted) should be (true)
    bColl2.toSeq.sorted.sameElements(bColl1.toSeq.sorted) should be (true)
  }

  test("unzip3") {
    val (aColl, bColl, iColl) = biHash.zipWithIndex.map { case (tuple, index) => (tuple._1, tuple._2, index) }.unzip3
    aColl.toSeq.sorted.sameElements((baseList map (_._1)).toSeq.sorted) should be (true)
    bColl.toSeq.sorted.sameElements((baseList map (_._2)).toSeq.sorted) should be (true)
    iColl.toSeq.sorted.sameElements((0 until biHash.size).toSeq.sorted) should be (true)
  }

  test("update(key, val)") {
    biHash update (baseList(0)._1, baseList(0)._2 + "!")
    biHash(baseList(0)._1) should equal (baseList(0)._2 + "!")
    biHash update (baseList(0)._2, baseList(0)._1 + 987)
    biHash(baseList(0)._2) should equal (baseList(0)._1 + 987)
  }

  test("updatedAB") {

    val bElem = baseList(0)._2 + "!"
    val updatedAB = biHash updated (baseList(0)._1, bElem)
    updatedAB(baseList(0)._1) should equal (bElem)

    val aElem = baseList(0)._1 + 987
    val updatedBA = biHash updated (baseList(0)._2, aElem)
    updatedBA(baseList(0)._2) should equal (aElem)

  }

  test("withDefault(that)") {

    val aFunc = (a: A) => a.toString + " is outta this world!"
    val deffy = biHash withDefault aFunc
    val badA  = -18

    deffy(baseList.head._1) should equal (baseList.head._2)
    deffy(badA) should equal (aFunc(badA))

  }

  test("withFilter(func)") {
    import collection.mutable.ListBuffer
    val abBuffer = new ListBuffer[AB]()
    biHash withFilter ((entry: AB) => entry._1 < (baseList map (_._1) max)) foreach (abBuffer += _)
    abBuffer.toList.size should equal (baseList.size - 1)
  }

  test("zip(that)") {
    val zipped = biHash zip (0 to baseList.size) map { case ((a, b), i) => (a, biHash.toList(i)._2 + "!")}
    zipped.size should equal (baseList.size)
    zipped.toSeq.sorted.sameElements(biHash.mapBs(_ + "!").toSeq.sorted) should be (true)
  }

  test("zipAll(that)") {
    val elem = 15 -> "Beedrill"
    val zipped = biHash zipAll (0 to baseList.size, elem, -1) map { case ((a, b), i) => (a, if (i < biHash.size) biHash.toList(i)._2 + "!" else elem._2 + "!")}
    zipped.size should equal (baseList.size + 1)
    zipped.toSeq.sorted.sameElements(biHash.mapBs(_ + "!").toSeq.sorted) should be (false)
    zipped.toSeq.sorted.sameElements((biHash + elem).mapBs(_ + "!").toSeq.sorted) should be (true)
  }

  test("zipWithIndex") {
    val elem = baseList(0)
    BiHashMap[A, B](elem).zipWithIndex.apply(elem) should equal (0)
  }

  // =============== UTILITIES ================

  // Why is it such a pain to test that two collections truly bear the same elements?!
  private def testSameElems[T <% { def toSeq: Seq[AB] }](coll1: T, coll2: T, target: Boolean = true) { coll1.toSeq.sorted.sameElements(coll2.toSeq.sorted) should be (target) }

}
