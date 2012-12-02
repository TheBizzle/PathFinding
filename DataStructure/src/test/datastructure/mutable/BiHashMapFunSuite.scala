package datastructure.mutable

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import collection.GenTraversableOnce
import org.scalatest.matchers.ShouldMatchers
import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:20 PM
 */

class BiHashMapFunSuite extends FunSuite with BeforeAndAfterEach with ShouldMatchers {

  type A = Int
  type B = String
  type BHM = BiHashMap[A, B]

  type AB = (A, B)
  type BA = (B, A)

  val aList: List[A] = List(5, 17, 1, 9, 4)
  val bList: List[B] = List("five", "seventeen", "one", "nine", "four")
  val baseList: List[(A, B)] = aList zip bList
  val biHash = BiHashMap[A, B]()

  override def beforeEach() {
    super.beforeEach()
    biHash.clear()
    biHash ++= baseList
  }

  test("==") {
    (biHash == BiHashMap[Double, B]()) should equal (false)
    (biHash == BiHashMap((aList tail) zip (bList tail) map { case (i, s) => (i.toDouble, s) }: _*)) should equal (false)
    (biHash == BiHashMap(baseList map { case (i, s) => (i.toDouble, s) }: _*)) should equal (false)
    (biHash == BiHashMap[A, B]()) should equal (false)
    (biHash == BiHashMap((aList tail) zip (bList tail): _*)) should equal (false)
    (biHash == BiHashMap(baseList: _*)) should equal (true)
  }

  test("!=") {
    (biHash != BiHashMap[Double, B]()) should equal (true)
    (biHash != BiHashMap((aList tail) zip (bList tail) map { case (i, s) => (i.toDouble, s) }: _*)) should equal (true)
    (biHash != BiHashMap(baseList map { case (i, s) => (i.toDouble, s) }: _*)) should equal (true)
    (biHash != BiHashMap[A, B]()) should equal (true)
    (biHash != BiHashMap((aList tail) zip (bList tail): _*)) should equal (true)
    (biHash != BiHashMap(baseList: _*)) should equal (false)
  }

  //@
  test("+(elem)") {
    def forwards(bhm: BHM, target: BHM, elems: AB*) {
      bhm + elems(0) should equal (target)
      bhm + elems(1) should equal (target)
    }
    def backwards(bhm: BHM, target: BHM, elems: BA*) {
      bhm + elems(0) should equal (target)
      bhm + elems(1) should equal (target)
    }
    val myElems = Seq(9001 -> "nein tousend won", 4 -> "fier")
    forwards (biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  //@
  test("+(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: AB*) {
      bhm + (elems(0), elems(1), elems.splitAt(2)._2: _*) should equal (target)
    }
    def backwards(bhm: BHM, target: BHM, elems: BA*) {
      bhm + (elems(0), elems(1), elems.splitAt(2)._2: _*) should equal (target)
    }
    val myElems = Seq(9001 -> "nein tousend won", 4 -> "fier", 3 -> "shree, akchurry")
    forwards (biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  //@
  test("++(genTraversableOnce[(A, B)])") {
    def forwards(bhm: BHM, target: GenTraversableOnce[(A, B)], appendee: GenTraversableOnce[(A, B)]) {
      testFunc(bhm, bhm, bhm)
      testFunc(bhm, target, appendee)
    }
    def backwards(bhm: BHM, target: GenTraversableOnce[(A, B)], appendee: GenTraversableOnce[(B, A)]) {
      testFunc(bhm, bhm, bhm.flip)
      testFunc(bhm, target, appendee)
    }
    def testFunc[T, U](bhm: BHM, target: GenTraversableOnce[(A, B)], appendee: GenTraversableOnce[(T, U)]) {
      bhm ++ appendee should equal (target)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards (biHash.clone, List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone, List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  //@
  //@ Can share with above
  test("++:(traversableOnce[(A, B)])") {
    def forwards(bhm: BHM, target: Traversable[(A, B)], appendee: Traversable[(A, B)]) {
      testFunc(bhm, bhm, bhm)
      testFunc(bhm, target, appendee)
    }
    def backwards(bhm: BHM, target: Traversable[(A, B)], appendee: Traversable[(B, A)]) {
      testFunc(bhm, bhm, bhm.flip)
      testFunc(bhm, target, appendee)
    }
    def testFunc[T, U](bhm: BHM, target: Traversable[(A, B)], appendee: Traversable[(T, U)]) {
      bhm ++: appendee should equal (target)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards (biHash.clone, List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone, List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  //@
  test("++=(traversableOnce[(A, B)])") {

    val original = biHash.clone
    original ++= original
    original should equal (biHash) //@ Should it really?

    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    (biHash.clone ++= myList)                should equal (List((biHash.toSeq ++ myList): _*))
    (biHash.clone ++= (myList map (_.swap))) should equal (List((biHash.toSeq ++ myList): _*))

  }

  // Cannot be `forwards`/`backwards`ized
  test("+=(elem)") {
    val myElem = 9001 -> "was?! mein thousand?!!?!?!?!"
    (biHash.clone += myElem)      should equal (BiHashMap(myElem :: baseList: _*))
    (biHash.clone += myElem.swap) should equal (BiHashMap(myElem :: baseList: _*))
  }

  test("+=(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: AB*) {
      (bhm += (elems(0), elems(1), elems.splitAt(2)._2: _*)); bhm should equal (target)
    }
    def backwards(bhm: BHM, target: BHM, elems: BA*) {
      (bhm += (elems(0), elems(1), elems.splitAt(2)._2: _*)); bhm should equal (target)
    }
    val myElems = List(9001 -> "was?! mein thousand?!!?!?!?!", 9002 -> "ja, dein thousand!", 91124 -> "no wai!", 90210 -> "yahweh!")
    forwards (biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone, BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  //@
  test("-(elem)") {
    def forwards(bhm: BHM, targetElemPairs: (BHM, A)*) {
      targetElemPairs foreach { case (target, elem) => bhm - elem; bhm should equal (target) }
    }
    def backwards(bhm: BHM, targetElemPairs: (BHM, B)*) {
      targetElemPairs foreach { case (target, elem) => bhm - elem; bhm should equal (target) }
    }
    val myPairs = baseList.tails.toSeq drop 1 map (BiHashMap[A, B](_: _*)) zip baseList
    forwards (biHash.clone, myPairs map { case (target, elem) => (target, elem._1) }: _*)
    backwards(biHash.clone, myPairs map { case (target, elem) => (target, elem._2) }: _*)
  }

  //@
  test("-(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: A*) {
      bhm - (elems(0), elems(1), elems.splitAt(2)._2: _*); bhm should equal (target)
    }
    def backwards(bhm: BHM, target: BHM, elems: B*) {
      bhm - (elems(0), elems(1), elems.splitAt(2)._2: _*); bhm should equal (target)
    }
    val (removables, pretarget) = baseList splitAt (baseList.size - 1)
    forwards (biHash.clone, BiHashMap(pretarget: _*), removables map (_._1): _*)
    backwards(biHash.clone, BiHashMap(pretarget: _*), removables map (_._2): _*)
  }

  //@
  //@ Refactor this and following test to call same code that generalizes over `GenTraversableOnce`
  test("--(that)") {
    def forwards(bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[A]) {
      (bhm -- bhm.aValues) should equal (BiHashMap.empty)
      (bhm -- removees)    should equal (target)
    }
    def backwards(bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[B]) {
      (bhm -- bhm.bValues) should equal (BiHashMap.empty)
      (bhm -- removees)    should equal (target)
    }
    val myList = baseList dropRight 2
    forwards (biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._1))
    backwards(biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._2))
  }

  //@
  //@ The behavior of `--=` is entirely wrong, even if the test passes.  The same goes for most of the other `x=` operators, I think.  They need to mutate the collection!
  test("--=(traversableOnce[A])") {
    def forwards(bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[A]) {
      (bhm --= bhm.aValues) should equal (BiHashMap.empty)
      (bhm --= removees)    should equal (target)
    }
    def backwards(bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[B]) {
      (bhm --= bhm.bValues) should equal (BiHashMap.empty)
      (bhm --= removees)    should equal (target)
    }
    val myList = baseList dropRight 2
    forwards (biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._1))
    backwards(biHash.clone, List(biHash.toSeq filterNot (myList contains): _*), myList map (_._2))
  }

  //@
  test("-=(elem)") {
    def forwards(bhm: BHM, target: BHM, elem: (A, B)) {
      (bhm -= elem._1); bhm should equal (target)
    }
    def backwards(bhm: BHM, target: BHM, elem: (B, A)) {
      (bhm -= elem._1); bhm should equal (target)
    }
    val myElem = baseList(1)
    val pretarget = baseList filterNot(_ == myElem)
    forwards (biHash.clone, BiHashMap(pretarget: _*), myElem)
    backwards(biHash.clone, BiHashMap(pretarget: _*), myElem.swap)
  }

  //@
  test("-=(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: A*) {
      (bhm -= (elems(0), elems(1), elems.splitAt(2)._2: _*)); bhm should equal (target)
    }
    def backwards(bhm: BHM, target: BHM, elems: B*) {
      (bhm -= (elems(0), elems(1), elems.splitAt(2)._2: _*)); bhm should equal (target)
    }
    val (removables, pretarget) = baseList splitAt (baseList.size - 1)
    forwards (biHash.clone, BiHashMap(pretarget: _*), removables map (_._1): _*)
    backwards(biHash.clone, BiHashMap(pretarget: _*), removables map (_._2): _*)
  }

  test("->") {
    biHash -> 2 should equal ((biHash, 2))
  }

  //@
  //@ Can share with all folds
  //@ Refactor to have this and `foldRight` pointing at the same test code
  // Cryptic symbol for "foldLeft"
  test("/:") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _))
    biHash.clone./:((1, ""))(f) should equal (expected)
  }

  //@
  //@ Can share with all folds
  //@ Refactor to have this and `foldRight` pointing at the same test code
  // Cryptic symbol for "fold"
  test("""/:\"""") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduce (_ + _))
    biHash.clone./:\((1, ""))(f) should equal (expected)
  }

  //@
  //@ Can share with all folds
  //@ Refactor to have this and `foldRight` pointing at the same test code
  // Cryptic symbol for "foldRight"
  test(""":\""") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _))
    biHash.clone.:\((1, ""))(f) should equal (expected)
  }

  //@ Can share with `bIterator`
  test("aIterator") {
    biHash.aIterator.toList should have ('sameElements (baseList.map(_._1)))
  }

  //@ Can refactor
  test("addString(sb, sep, start, end)") {
    val (sep, start, end) = ("york", "dork", "bjork")
    val target1 = baseList.addString(new StringBuilder()).toString replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    val target2 = baseList.addString(new StringBuilder(), sep).toString replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    val target4 = baseList.addString(new StringBuilder(), sep, start, end).toString replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    biHash.addString(new StringBuilder()).toString should equal (target1)                    // One-arg version
    biHash.addString(new StringBuilder(), sep).toString should equal (target2)               // Two-arg version
    biHash.addString(new StringBuilder(), sep, start, end).toString should equal (target4)   // Four-arg version
  }

  //@
  //@ Can share code with the folds
  test("aggregate(b)(seqop, combop)") {
    val expected = biHash map (_._1) product
    val seqF   = (acc: A, elem: AB) => acc * elem._1
    val comboF = (_: A, _: A) => -1
    biHash.clone.aggregate(1)(seqF, comboF) should equal (expected)
  }

  //@ Refactor
  test("andThen(func)") {

    val bFunc: (B) => B = (b: B) => identity(b)
    biHash andThen bFunc apply baseList(0)._1 should equal (baseList(0)._2)

    val aFunc: (A) => A = (a: A) => identity(a)
    biHash andThen aFunc apply baseList(0)._2 should equal (baseList(0)._1)

  }

  //@
  test("apply(elem)") {
    biHash(baseList(0)._1) should equal (baseList(0)._2)
    biHash(baseList(1)._2) should equal (baseList(1)._1)
  }

  //@ Can share with `bSet`
  test("aSet") {
    biHash.aSet should have ('sameElements (baseList.map(_._1)))
  }

  test("bIterator") {
    biHash.bIterator.toList should have ('sameElements (baseList.map(_._2)))
  }

  test("bSet") {
    biHash.bSet should have ('sameElements (baseList.map(_._2)))
  }

  test("clear()") {
    biHash.clear() should equal (BiHashMap[A, B]())
  }

  test("clone") {
    biHash.clone should equal (biHash)
  }

  //@
  //@ Can share below
  //@ Refactor
  test("collect(pf)") {
    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val aComparator = (a: A) => a < aAverage
    biHash collect { case (a: A, b: B) if (a == null) => b }           should equal (List())                                                        // Match (none)
    biHash collect { case (a: A, b: B) if (a == abList.head._1) => b } should equal (List(abList.head._2))                                          // Match (one)
    biHash collect { case (a: A, b: B) if (aComparator(a)) => b }      should equal (List(abList.filter(entry => aComparator(entry._1)).toSeq: _*)) // Match (some)
  }

  //@
  test("collectFirst(pf)") {
    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val aComparator = (a: A) => a < aAverage
    biHash collectFirst { case (a: A, b: B) if (a == null) => b }           should equal (None)                                                      // Match (none)
    biHash collectFirst { case (a: A, b: B) if (a == abList.head._1) => b } should equal (Some(abList.head._2))                                      // Match (one)
    biHash collectFirst { case (a: A, b: B) if (aComparator(a)) => b }      should equal (Some(abList filter (entry => aComparator(entry._1)) head)) // Match (some)
  }

  //@
  //@ Refactor
  test("compose(func)") {

    val aFunc: (A) => A = (a: A) => a + 1
    intercept[NoSuchElementException] { biHash compose aFunc apply baseList.minBy(_._1)._1 }
    (biHash compose aFunc apply baseList.minBy(_._1)._1 - 1) should equal (baseList.minBy(_._1)._2)

    val bFunc: (B) => B = (b: B) => new String(b.getBytes map (x => (x ^ 2).toByte))
    intercept[NoSuchElementException] { biHash compose bFunc apply baseList.head._2 }
    biHash compose bFunc apply bFunc(baseList.head._2) should equal (baseList.head._1)

  }

  //@
  test("contains(elem)") {

    val badA = 3421
    val badB = "I'm bad!  Let's go eat some cheeseburgers with President Ronnie!"

    biHash should not have ('contains (badA))
    biHash should not have ('contains (badB))

    biHash should have ('contains (baseList.head._1))
    biHash should have ('contains (baseList.head._2))

  }

  test("copyToArray(arr)") {

    val abArr = new Array[AB](biHash.size)
    biHash.copyToArray[AB](abArr)
    abArr should equal (biHash.toArray)

    val baArr = new Array[BA](biHash.size)
    biHash.copyToArray[BA](baArr)
    baArr should equal (biHash.flip.toArray)

  }

  test("copyToArray(arr, start)") {

    val start = 2

    val abArr = new Array[AB](biHash.size - start)
    biHash.copyToArray[AB](abArr, start)
    abArr should equal (biHash.toArray drop (start))

    val baArr = new Array[BA](biHash.size - start)
    biHash.copyToArray[BA](baArr, start)
    baArr should equal (biHash.flip.toArray drop (start))

  }

  test("copyToArray(arr, start, len)") {

    val (start, len) = (2, 2)

    val abArr = new Array[AB](len)
    biHash.copyToArray[AB](abArr, start, len)
    abArr should equal (biHash.toArray slice (start, start + len))

    val baArr = new Array[BA](len)
    biHash.copyToArray[BA](baArr, start, len)
    baArr should equal (biHash.flip.toArray slice (start, start + len))

  }

  //@Refactor (along with the 3 above)
  test("copyToBuffer(buff)") {

    val abBuff = new ListBuffer[AB]()
    biHash.copyToBuffer[AB](abBuff) //@@ Note: Another place where this fails is when method type parameter annotations like `C >: (A, B)` and `C >: (B, A)` have to be used and it must decide which to pick
    abBuff.toList should equal (biHash.toList)

    val baBuff = new ListBuffer[BA]()
    biHash.copyToBuffer[BA](baBuff)
    baBuff.toList should equal (biHash.toList)

  }

  //@
  //@ Refactor
  test("count(func)") {
    val abFunc = (ab: AB, a: A) => ab._1 < a
    val abList = baseList
    abList map (_._1) foreach (x => biHash count (abFunc(_, x)) should equal (abList count (abFunc(_, x))))
  }

  //@
  //@ Refactor
  test("default(elem)") {

    val aDef = 4879
    intercept [NoSuchElementException] { biHash.default(aDef) }

    val bDef = "alksdalsdk"
    intercept [NoSuchElementException] { biHash.default(bDef) }

  }

  //@ Should share code with the two below
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

    val allCleansed = biHash takeRight (baseList.size + 10)
    allCleansed.size should equal (0)

  }

  //@
  test("dropWhile(func)") {

    val abCleansed = biHash dropWhile (_._1 <= (baseList map (_._1) max))
    abCleansed.size should equal (0)

    var counter = 0
    val cleansed = biHash takeWhile { case _ => counter += 1; counter < baseList.size }
    cleansed.size should equal (1)

  }

  test("empty") {
    biHash.empty should equal (BiHashMap[A, B]())
  }

  //@ Can share code with below
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
    biHash should not equal (biHash += elem)
    (biHash += elem) should equal (biHash += elem)
  }

  //@
  //@ Refactor
  test("exists(func)") {
    biHash.exists(_ == 3421) should equal (false)
    biHash.exists(_ == baseList.head._1) should equal (true)
  }

  //@
  //@ Refactor
  //@ Share code with some below
  test("filter(func)") {
    biHash filter (_._1 == 3421)             should equal (biHash)
    biHash filter (_._1 == baseList.head._1) should equal (BiHashMap(baseList.head))
  }

  //@ Refactor
  //@ Share with below
  test("filterAs(func)") {
    biHash filterAs (a => baseList map (_._2) contains (a)) should equal (biHash.empty)
    biHash filterAs (a => baseList map (_._1) contains (a)) should equal (biHash)
    biHash filterAs (_ == baseList.head._1)      should equal (BiHashMap(baseList.head))
    biHash filterAs (_ != baseList.tail.head._1) should equal (BiHashMap((baseList.head :: baseList.tail.tail): _*))
  }

  //@ Refactor
  test("filterBs(func)") {
    biHash filterBs (b => baseList map (_._1) contains (b)) should equal (biHash.empty)
    biHash filterBs (b => baseList map (_._2) contains (b)) should equal (biHash)
    biHash filterBs (_ == baseList.head._2)      should equal (BiHashMap(baseList.head))
    biHash filterBs (_ != baseList.tail.head._2) should equal (BiHashMap((baseList.head :: baseList.tail.tail): _*))
  }

  //@
  //@ Refactor
  test("filterNot(func)") {
    biHash filterNot (_._1 != 3421)             should equal (biHash)
    biHash filterNot (_._1 != baseList.head._1) should equal (BiHashMap(baseList.head))
  }

  //@
  //@ Refactor
  //@ Share code with above
  test("find(func)") {
    biHash find (_._1 != 3421)             should equal (None)
    biHash find (_._1 != baseList.head._1) should equal (Some(baseList.head))
  }

  //@
  //@ Share code with `map`
  test("flatMap(func)") {
    val f = (entry: AB) => (entry._1 * 6, entry._2)
    val g = (entry: AB) => BiHashMap(f(entry))
    (biHash flatMap g) should equal (BiHashMap((baseList map f): _*))
  }

  test("flip") {
    biHash.flip should equal (BiHashMap(baseList map (_.swap): _*))
  }

  //@
  test("fold(res)(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduce (_ + _))
    biHash.clone.fold((1, ""))(f) should equal (expected)
  }

  //@
  test("foldLeft(res)(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _))
    biHash.clone.foldLeft((1, ""))(f) should equal (expected)
  }

  //@
  test("foldRight(res)(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _))
    biHash.clone.foldRight((1, ""))(f) should equal (expected)
  }

  //@
  //@ Refactor
  test("forall(func)") {
    biHash forall (baseList.contains(_))     should be (true)
    biHash forall (_._2 == "five")           should be (false)
    biHash forall (_._2 == "lkjhadskjhsadl") should be (false)
  }

  //@
  //@ Refactor
  test("foreach(func)") {

    val accInit = (0, "")
    var acc     = accInit
    val aOp     = (a: A) => a * 2
    val aFunc   = (a: A) => acc._1 + aOp(a)
    val bOp     = (b: B) => b + "!"
    val bFunc   = (b: B) => acc._2 + bOp(b)
    val abFunc  = (ab: (A, B)) => (aFunc(ab._1), bFunc(ab._2))

    biHash foreach { case (a: A, b: B) => acc = abFunc((a, b)) }
    acc should equal ((aList map aOp sum), (bList map bOp reduce (_ + _)))

  }

  test("formatted(str)") {
    val formatStr = "Herp, %s, I'm a derp."
    biHash.formatted(formatStr) should equal (formatStr.format(biHash.toString))
  }

  //@
  //@ Can share code with `apply` (and also some with `getOrElse`)
  test("get(elem)") {
    biHash.get(baseList(0)._1) should equal (Option(baseList(0)._2))
    biHash.get(baseList(1)._2) should equal (Option(baseList(1)._1))
  }

  //@
  //@ Refactor
  //@ Can share code with below
  test("getOrElse(a, default)") {

    val badA = 98384
    val badB = "MY EMPORER!  I'VE FAAAAAAILED YOOOOOOU!"

    biHash getOrElse (baseList(2)._1, badB) should equal (baseList(2)._2)
    biHash getOrElse (badA, badB) should equal (badB)

    biHash getOrElse (baseList(2)._2, badA) should equal (baseList(2)._1)
    biHash getOrElse (badB, badA) should equal (badA)

  }

  //@
  //@ Refactor
  test("getOrElseUpdate(a, => b)") {

    val badA = 98384
    val badB = "MY EMPORER!  I'VE FAAAAAAILED YOOOOOOU!"

    biHash getOrElseUpdate (baseList(2)._1, badB) should equal (baseList(2)._2)
    biHash getOrElseUpdate (badA, badB) should equal (badB)

    biHash getOrElseUpdate (baseList(2)._2, badA) should equal (baseList(2)._1)
    biHash getOrElseUpdate (badB, 49994) should equal (badA)  // A prior line of test code puts `badB <-> badA` into the map

  }

  //@
  //@ Refactor
  // Yeah... try making sense of _this_ test
  test("groupBy(f)") {
    val newKV = baseList.head._1 * 1000 -> baseList.head._2
    biHash += newKV
    biHash groupBy (_._2) should equal (Map(baseList.head._2 -> BiHashMap(baseList.head, newKV)) ++ (baseList.tail map (x => x._2 -> BiHashMap(x))))
  }

  test("grouped(int)") {
    intercept[IllegalArgumentException] { biHash grouped (0) }
    (biHash grouped (1) toList) zip baseList foreach { case (x, y) => x should equal (BiHashMap(y)) }
    (biHash grouped (2) toList) zip (baseList grouped (2) toList) foreach { case (bhg, blg) => bhg should equal (BiHashMap(blg: _*)) }
  }

  test("hasDefiniteSize") {
    biHash.hasDefiniteSize should equal (true)
  }

  //@ Share code with below
  test("head") {
    baseList should have ('exists (biHash.head))
  }

  test("headOption") {
    baseList should have ('exists (biHash.headOption.get))
  }

  test("init") {
    biHash.init.size should equal (baseList.size - 1)
    biHash.init.toList.distinct should equal (baseList.size - 1)
  }

  test("inits") {
    def checkInits(bhm: BiHashMap[A, B], inits: Iterator[BiHashMap[A, B]]) {
      inits.next should equal (bhm)
      if (bhm.nonEmpty) checkInits(bhm.init, inits)
    }
    checkInits(biHash, biHash.inits)
  }

  //@
  //@ Refactor
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

  //@ Verify that this works
  test("isTraversableAgain") {
    biHash should be ('traversableAgain)
  }

  test("iterator") {
    biHash.iterator.toList should have ('sameElements (baseList))
  }

  //@ Share code with below
  test("last") {
    baseList should have ('exists (biHash.last))
  }

  test("lastOption") {
    baseList should have ('exists (biHash.lastOption.get))
  }

  //@
  //@ Refactor
  test("map(func)") {
    val func = (entry: (A, B)) => (entry._1 * 6, entry._2)
    (biHash map func) should equal (BiHashMap((baseList map func): _*))
  }

  //@ Refactor
  //@ Share code with below
  test("mapAs") {
    biHash mapAs (_.toDouble) should have ('sameElements (biHash map { case (a: A, b: B) => (a.toDouble, b) } ))
    biHash.empty mapAs (_.toDouble) should equal (biHash.empty)
  }

  test("mapBs") {
    biHash mapBs (_.getBytes) should have ('sameElements (biHash map { case (a: A, b: B) => (a, b.getBytes) } ))
    biHash.empty mapBs (_.getBytes) should equal (biHash.empty)
  }

  //@
  //@ Refactor
  test("mapResult") {
    val func = (entry: (A, B)) => (entry._1, entry._2.getBytes)
    biHash.mapResult(_ map func).result() should equal (BiHashMap(baseList map func: _*))
  }

  //@
  //@ Can be merged with `minBy`
  test("maxBy") {
    biHash maxBy (_._1) should equal (baseList maxBy (_._1))
  }

  //@
  test("minBy") {
    biHash minBy (_._1) should equal (baseList.minBy(_._1))
  }

  // All three variants
  //@ Can refactor
  //@ Can share code with `addString`
  test("mkString") {
    val (sep, start, end) = ("york", "dork", "bjork")
    val target0 = baseList.mkString
    val target1 = baseList.mkString(sep)
    val target3 = baseList.mkString(sep, start, end) replaceAll (",", " -> ") replaceAll ("""\(|\)""", "")
    biHash.mkString should equal (target0)                    // No-arg version
    biHash.mkString(sep) should equal (target1)               // One-arg version
    biHash.mkString(sep, start, end) should equal (target3)   // Three-arg version
  }

  test("nonEmpty") {
    biHash should be ('nonEmpty)
    BiHashMap[A, B]() should not be ('nonEmpty)
  }

  //@
  //@ Refactor
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

  //@
  //@ Refactor
  test("partition(func)") {
    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val abComparator = (ab: (A, B)) => (ab._1 < aAverage)
    biHash.partition(_ == null)              should equal ((biHash.empty, biHash))                                                                        // Match (none, all)
    biHash.partition(_._1 == abList.head._1) should equal ((BiHashMap(abList.head), BiHashMap(abList.tail: _*)))                                          // Match (one, rest)
    biHash.partition(abComparator)           should equal ((BiHashMap(abList filter (abComparator): _*), BiHashMap(abList filterNot (abComparator): _*))) // Match (some, others)
  }

  //@
  //@ Can probably share code with the += stuff
  test("put(elem)") {
    val elem  = 9001 -> "was?! mein thousand?!!?!?!?!"
    val myMap = biHash.clone
    (myMap.put(elem._1, elem._2)).get should equal (elem._2) //@@ Note: Part of why my hack is broken: `myMap.put _` is a compiler error, since it doesn't know which to choose; essentially, tupling and currying are useless with most of these methods
    myMap should equal (BiHashMap(elem :: baseList: _*))
  }

  //@
  test("reduce(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduce (_ + _))
    biHash.clone reduce f should equal (expected)
  }

  //@
  test("reduceLeft(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _))
    biHash.clone reduceLeft f should equal (expected)
  }

  //@
  //@ Share with `reduceLeft`
  test("reduceLeftOption(func)") {

    def testFunc(bhm: BHM, target: Option[AB], func: (AB, AB) => AB) {
      (bhm reduceLeftOption func) should equal (target)
    }

    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expectedSome = Option((biHash map (_._1) product, biHash map (_._2) reduceLeft (_ + _)))

    testFunc(biHash.clone, expectedSome, f)
    testFunc(biHash.empty, None,         f)

  }

  //@
  //@ Share code with `reduce`
  test("reduceOption(func)") {

    def testFunc(bhm: BHM, target: Option[AB], func: (AB, AB) => AB) {
      (bhm reduceOption func) should equal (target)
    }

    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expectedSome = Option((biHash map (_._1) product, biHash map (_._2) reduce (_ + _)))

    testFunc(biHash.clone, expectedSome, f)
    testFunc(biHash.empty, None,         f)

  }

  //@
  test("reduceRight(func)") {
    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expected = (biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _))
    biHash.clone reduceRight f should equal (expected)
  }

  //@
  test("reduceRightOption(func)") {

    def testFunc(bhm: BHM, target: Option[AB], func: (AB, AB) => AB) {
      (bhm reduceRightOption func) should equal (target)
    }

    val f = (tup1: AB, tup2: AB) => (tup1._1 * tup2._1, tup1._2 + tup2._2)
    val expectedSome = Option((biHash map (_._1) product, biHash map (_._2) reduceRight (_ + _)))

    testFunc(biHash.clone, expectedSome, f)
    testFunc(biHash.empty, None,         f)

  }

  //@
  //@ Refactor
  test("remove(elem)") {
    biHash(baseList(0)._1) should equal (baseList(0)._2)
    biHash(baseList(1)._2) should equal (baseList(1)._1)
    biHash.remove(baseList(0)._1).get should equal (None)
    biHash.remove(baseList(1)._2).get should equal (None)
  }

  //@
  //@ Refactor within self
  test("retain(func)") {

    val headMapAB = biHash.retain { case (a, b) => (a, b) == biHash.head }
    headMapAB should have ('sameElements (BiHashMap(biHash.head)))

    val tailMapAB = biHash.retain { case (a, b) => biHash.tail.exists(_ == (a, b)) }
    tailMapAB should have ('sameElements (BiHashMap(biHash.tail.toSeq: _*)))

    val allMapAB = biHash.retain((_, _) => true)
    allMapAB should have ('sameElements (biHash))

  }

  //@ Be smart when implementing this!
  test("sameElements(that)") {
    biHash should have ('sameElements (baseList))
    biHash.flip should have ('sameElements (baseList))
    biHash should not have ('sameElements (biHash.empty))
    biHash.empty should have ('sameElements (BiHashMap[A, B]()))
    biHash.empty should have ('sameElements (BiHashMap[B, A]()))
  }

  //@
  //@ Share code with the two below
  test("scan(res)(func)") {
    val (abBase, abFunc) = ((0, ""), (x: (A, B), y: (A, B)) => (x._1 + y._1, x._2 + y._2))
    biHash.scan(abBase)(abFunc) should have ('sameElements (baseList.scan(abBase)(abFunc)))
  }

  //@
  test("scanLeft(res)(func)") {
    val (abBase, abFunc) = ((0, ""), (x: (A, B), y: (A, B)) => (x._1 + y._1, x._2 + y._2))
    biHash.scanLeft(abBase)(abFunc) should have ('sameElements (baseList.scanLeft(abBase)(abFunc)))
  }

  //@
  test("scanRight(res)(func)") {
    val (abBase, abFunc) = ((0, ""), (x: (A, B), y: (A, B)) => (x._1 + y._1, x._2 + y._2))
    biHash.scanRight(abBase)(abFunc) should have ('sameElements (baseList.scanRight(abBase)(abFunc)))
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

  //@
  //@ Refactor for code sharing within self
  test("span(func)") {

    val (preAB1, postAB1) = biHash.span(baseList.contains(_))
    preAB1 should have ('sameElements (baseList))
    postAB1 should have size (0)

    val (preAB2, postAB2) = biHash.span(!baseList.contains(_))
    preAB2 should have size (0)
    postAB2 should have ('sameElements (baseList))

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

  //@ Share with `flip`
  test("swap") {
    biHash.swap should equal (BiHashMap(baseList map (_.swap): _*))
  }

  test("tail") {
    biHash.tail.size should equal (baseList.size - 1)
    biHash.tail.toList.distinct should equal (baseList.size - 1)
  }

  test("tails") {
    def checkTails(bhm: BiHashMap[A, B], tails: Iterator[BiHashMap[A, B]]) {
      tails.next should equal (bhm)
      if (bhm.nonEmpty) checkTails(bhm.tail, tails)
    }
    checkTails(biHash, biHash.tails)
  }

  //@ Can share code with below
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

  //@
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
    baseList.toArray should have ('sameElements (biHash.toArray))
  }

  test("toBuffer") {
    baseList.toBuffer should have ('sameElements (biHash.toBuffer))
  }

  test("toIndexedSeq") {
    baseList.toIndexedSeq should have ('sameElements (biHash.toIndexedSeq))
  }

  test("toIterable") {
    baseList.toIterable should have ('sameElements (biHash.toIterable))
  }

  //@ Why doesn't `should` work here?
  test("toIterator") {
    baseList.toIterator.sameElements(biHash.toIterator) should be (true)
  }

  test("toList") {
    baseList.sameElements(biHash.toList) should be (true)
  }

  test("toMap") {
    baseList.toMap should equal (biHash.toMap)
  }

  test("toSeq") {
    baseList.toSeq should have ('sameElements (biHash.toSeq))
  }

  test("toSet") {
    baseList.toSet should equal (biHash.toSet)
  }

  test("toStream") {
    baseList.toStream should have ('sameElements (biHash.toStream))
  }

  test("toTraversable") {
    baseList.toTraversable should equal (biHash.toTraversable)
  }

  //@
  test("transform(func)") {
    val abFunc: (A, B) => B = (k: A, v: B) => v + "_" + k.toString
    val abTransformed = biHash transform abFunc
    abTransformed should equal (BiHashMap((baseList map { case (k, v) => (k, (abFunc(k, v))) }): _*))
  }

  test("unzip") {
    val (aColl1, bColl1) = biHash.unzip
    val (aColl2, bColl2) = baseList.unzip
    aColl2 should have ('sameElements (aColl1))
    bColl2 should have ('sameElements (bColl1))
  }

  //@
  test("unzip3") {
    val (aColl, bColl, iColl) = biHash.zipWithIndex.map { case (tuple, index) => (tuple._1, tuple._2, index) }.unzip3
    aColl should have ('sameElements (baseList map (_._1)))
    bColl should have ('sameElements (baseList map (_._2)))
    iColl should have ('sameElements (0 until biHash.size))
  }

  //@
  //@ Refactor
  //@ Can share code with below
  test("update(key, val)") {
    biHash update (baseList(0)._1, baseList(0)._2 + "!")
    biHash(baseList(0)._1) should equal (baseList(0)._2 + "!")
    biHash update (baseList(0)._2, baseList(0)._1 + 987)
    biHash(baseList(0)._2) should equal (baseList(0)._1 + 987)
  }

  //@
  //@ Refactor
  //@ Can share code with above
  test("updated(a, b)") {

    val bElem = baseList(0)._2 + "!"
    val updatedAB = biHash updated (baseList(0)._1, bElem)
    updatedAB(baseList(0)._1) should equal (bElem)

    val aElem = baseList(0)._1 + 987
    val updatedBA = biHash updated (baseList(0)._2, aElem)
    updatedBA(baseList(0)._2) should equal (aElem)

  }

  //@ Refactor
  //@ Is currently inoperable (must fix return type)
  test("withDefault(that)") {

//    val deffy = biHash withDefaultA { case b: B => b.size }
//    val badB  = "jkashdgkjlashdgkljsdagh"
//    deffy(baseList.head._2) should equal (baseList.head._1)
//    deffy(badB) should equal (badB.size)

    //@ WTF is going on here...?
    val aFunc = (a: A) => a.toString + " is outta this world!"
    val deffy = biHash withDefault aFunc
    val badA  = -18

    deffy(baseList.head._1) should equal (baseList.head._2)
    deffy(badA) should equal (aFunc(badA))

  }

  //@
  test("withFilter(func)") {
    import collection.mutable.ListBuffer
    val abBuffer = new ListBuffer[(A, B)]()
    biHash withFilter ((entry: (A, B)) => entry._1 < (baseList map (_._1) max)) foreach (abBuffer += _)
    abBuffer.toList.size should equal (baseList.size - 1)
  }

  //@ Can share code with below
  test("zip(that)") {
    val zipped = (biHash zip (0 to baseList.size))
    zipped.size should equal (baseList.size)
    zipped should equal (BiHashMap[(A, B), Int](baseList.zipWithIndex: _*))
  }

  test("zipAll(that)") {
    val elem = 15 -> "Beedrill"
    val zipped = (biHash zipAll (0 to baseList.size, elem, -1))
    zipped.size should equal (baseList.size + 1)
    zipped should not equal (BiHashMap[(A, B), Int](baseList.zipWithIndex: _*))
    zipped should equal (BiHashMap[(A, B), Int]((elem :: baseList).zipWithIndex: _*))
  }

  test("zipWithIndex") {
    val elem = baseList(0)
    BiHashMap[A, B](elem).zipWithIndex.apply(elem) should equal (0)
  }

}
