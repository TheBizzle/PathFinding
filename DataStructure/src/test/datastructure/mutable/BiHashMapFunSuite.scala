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

  // Type disjunction, courtesy of the great Miles Sabin
  type ~[X]     = X => Nothing
  type |[X, Y]  = ~[~[X] with ~[Y]]
  type ~~[X]    = ~[~[X]]
  type ||[X, Y] = { type T[L] = ~~[L] <:< (X | Y) }

  type AB      = (A || B)#T
  type ABTuple = ((A, B) || (B, A))#T

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

  //?
  //@
  test("+(elem)") {
    def forwards(bhm: BHM, target: BHM, elems: (A, B)*) {
      testFunc(bhm, target, elems: _*)
    }
    def backwards(bhm: BHM, target: BHM, elems: (B, A)*) {
      testFunc(bhm, target, elems: _*)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elems: (T, U)*) {
      bhm + elems(0) should equal (target)
      bhm + elems(1) should equal (target)
    }
    val myElems = Seq(9001 -> "nein tousend won", 4 -> "fier")
    forwards (biHash.clone(), BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone(), BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  //@
  test("+(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: (A, B)*) {
      testFunc(bhm, target, elems: _*)
    }
    def backwards(bhm: BHM, target: BHM, elems: (B, A)*) {
      testFunc(bhm, target, elems: _*)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elems: (T, U)*) {
      bhm + (elems(0), elems(1), elems.splitAt(2)._2: _*) should equal (target)
    }
    val myElems = Seq(9001 -> "nein tousend won", 4 -> "fier", 3 -> "shree, akchurry")
    forwards (biHash.clone(), BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone(), BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  //@
  test("++(that)") {
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
    forwards (biHash.clone(), List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone(), List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  //@
  //@ Can share with above
  test("++:(that)") {
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
    forwards (biHash.clone(), List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone(), List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  //@
  test("++=(that)") {
    def forwards(bhm: BHM, target: TraversableOnce[(A, B)], appendee: TraversableOnce[(A, B)]) {
      testFunc(bhm, target, appendee)
    }
    def backwards(bhm: BHM, target: TraversableOnce[(A, B)], appendee: TraversableOnce[(B, A)]) {
      testFunc(bhm, target, appendee)
    }
    def testFunc[T, U](bhm: BHM, target: TraversableOnce[(A, B)], appendee: TraversableOnce[(T, U)]) {
      val orig = bhm.clone(); bhm ++= bhm; orig should equal (bhm)
      bhm ++= appendee; bhm should equal (target)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards (biHash.clone(), List(biHash.toSeq ++ myList: _*), myList)
    backwards(biHash.clone(), List(biHash.toSeq ++ myList: _*), myList map (_.swap))
  }

  test("+=(elem)") {
    def forwards(bhm: BHM, target: BHM, elem: (A, B)) {
      testFunc(bhm, target, elem)
    }
    def backwards(bhm: BHM, target: BHM, elem: (B, A)) {
      testFunc(bhm, target, elem)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elem: (T, U)) {
      bhm += elem; bhm should equal (target)
    }
    val myElem = 9001 -> "was?! mein thousand?!!?!?!?!"
    forwards (biHash.clone(), BiHashMap(myElem :: baseList: _*), myElem)
    backwards(biHash.clone(), BiHashMap(myElem :: baseList: _*), myElem.swap)
  }

  test("+=(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: (A, B)*) {
      testFunc(bhm, target, elems: _*)
    }
    def backwards(bhm: BHM, target: BHM, elems: (B, A)*) {
      testFunc(bhm, target, elems: _*)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elems: (T, U)*) {
      (bhm += (elems(0), elems(1), elems.splitAt(2)._2: _*)); bhm should equal (target)
    }
    val myElems = List(9001 -> "was?! mein thousand?!!?!?!?!", 9002 -> "ja, dein thousand!", 91124 -> "no wai!", 90210 -> "yahweh!")
    forwards (biHash.clone(), BiHashMap(baseList ++ myElems: _*), myElems: _*)
    backwards(biHash.clone(), BiHashMap(baseList ++ myElems: _*), myElems map (_.swap): _*)
  }

  //@
  test("-(elem)") {
    def forwards(bhm: BHM, targetElemPairs: (BHM, (A, B))*) {
      testFunc(bhm, targetElemPairs: _*)
    }
    def backwards(bhm: BHM, targetElemPairs: (BHM, (B, A))*) {
      testFunc(bhm, targetElemPairs: _*)
    }
    def testFunc[T, U](bhm: BHM, targetElemPairs: (BHM, (T, U))*) {
      targetElemPairs foreach { case (target, elem) => bhm - elem; bhm should equal (target) }
    }
    val myPairs = baseList.tails.toSeq drop 1 map (BiHashMap[A, B](_: _*)) zip baseList
    forwards (biHash.clone(), myPairs: _*)
    backwards(biHash.clone(), myPairs map { case (target, elem) => (target, elem.swap) }: _*)
  }

  //@
  test("-(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: (A, B)*) {
      testFunc(bhm, target, elems: _*)
    }
    def backwards(bhm: BHM, target: BHM, elems: (B, A)*) {
      testFunc(bhm, target, elems: _*)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elems: (T, U)*) {
      bhm - (elems(0), elems(1), elems.splitAt(2)._2: _*); bhm should equal (target)
    }
    val (removables, pretarget) = baseList splitAt (baseList.size - 1)
    forwards (biHash.clone(), BiHashMap(pretarget: _*), removables: _*)
    backwards(biHash.clone(), BiHashMap(pretarget: _*), removables map (_.swap): _*)
  }

  //@
  //@ Refactor this and following test to call same code that generalizes over `GenTraversableOnce`
  test("--(that)") {
    def forwards(bhm: BHM, target: GenTraversableOnce[(A, B)], removees: GenTraversableOnce[A]) {
      testFunc(bhm, BiHashMap.empty, bhm)
      testFunc(bhm, target, removees)
    }
    def backwards(bhm: BHM, target: GenTraversableOnce[(A, B)], removees: GenTraversableOnce[B]) {
      testFunc(bhm, BiHashMap.empty, bhm.flip)
      testFunc(bhm, target, removees)
    }
    def testFunc[T](bhm: BHM, target: GenTraversableOnce[(A, B)], removees: GenTraversableOnce[T]) {
      bhm -- removees should equal (target)
    }
    val myList = baseList dropRight 2
    forwards (biHash.clone(), List(biHash.toSeq filterNot (myList contains): _*), myList map (_._1))
    backwards(biHash.clone(), List(biHash.toSeq filterNot (myList contains): _*), myList map (_._2))
  }

  //@
  test("--=(traversableonce[A])") {
    def forwards(bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[A]) {
      testFunc(bhm, BiHashMap.empty, bhm)
      testFunc(bhm, target, removees)
    }
    def backwards(bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[B]) {
      testFunc(bhm, BiHashMap.empty, bhm.flip)
      testFunc(bhm, target, removees)
    }
    def testFunc[T](bhm: BHM, target: TraversableOnce[(A, B)], removees: TraversableOnce[T]) {
      (bhm --= removees) should equal (target)
    }
    val myList = baseList dropRight 2
    forwards (biHash.clone(), List(biHash.toSeq filterNot (myList contains): _*), myList map (_._1))
    backwards(biHash.clone(), List(biHash.toSeq filterNot (myList contains): _*), myList map (_._2))
  }

  //@
  test("-=(elem)") {
    def forwards(bhm: BHM, target: BHM, elem: (A, B)) {
      testFunc(bhm, target, elem)
    }
    def backwards(bhm: BHM, target: BHM, elem: (B, A)) {
      testFunc(bhm, target, elem)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elem: (T, U)) {
      (bhm -= elem); bhm should equal (target)
    }
    val myElem = baseList(1)
    val pretarget = baseList filterNot(_ == myElem)
    forwards (biHash.clone(), BiHashMap(pretarget: _*), myElem)
    backwards(biHash.clone(), BiHashMap(pretarget: _*), myElem.swap)
  }

  //@
  test("-=(elem1, elem2, elems)") {
    def forwards(bhm: BHM, target: BHM, elems: (A, B)*) {
      testFunc(bhm, target, elems)
    }
    def backwards(bhm: BHM, target: BHM, elems: (B, A)*) {
      testFunc(bhm, target, elems)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elems: (T, U)*) {
      (bhm -= (elems(0), elems(1), elems.splitAt(2)._2: _*)); bhm should equal (target)
    }
    val (removables, pretarget) = baseList splitAt (baseList.size - 1)
    forwards (biHash.clone(), BiHashMap(pretarget: _*), removables: _*)
    backwards(biHash.clone(), BiHashMap(pretarget: _*), removables map (_.swap): _*)
  }

  test("->") {
    biHash -> 2 should equal ((biHash, 2))
  }

  //@
  //@ Can share with all folds
  //@ Refactor to have this and `foldRight` pointing at the same test code
  // Cryptic symbol for "foldLeft"
  test("/:") {
    def forwards(bhm: BHM, target: A, initializer: A, func: (A, (A, B)) => A) {
      testFunc(bhm, target, initializer, func)
    }
    def backwards(bhm: BHM, target: B, initializer: B, func: (B, (B, A)) => B) {
      testFunc(bhm, target, initializer, func)
    }
    def testFunc[T, U](bhm: BHM, target: T, initializer: T, func: (T, (T, U)) => T) {
      (bhm /: (initializer)(func)) should equal (target)
    }
    forwards (biHash.clone(), biHash map (_._1) product, 1, { case (acc, elem: (Int, String)) => acc * elem._1 })
    backwards(biHash.clone(), biHash map (_._2) mkString, "", { case (acc, elem: (String, Int)) => acc + elem._1 })
  }

  //@
  //@ Can share with all folds
  //@ Refactor to have this and `foldRight` pointing at the same test code
  // Cryptic symbol for "fold"
  test("""/:\"""") {
    def forwards(bhm: BHM, target: (A, B), initializer: (A, B), func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, initializer, func)
    }
    def backwards(bhm: BHM, target: (B, A), initializer: (B, A), func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, initializer, func)
    }
    def testFunc[T](bhm: BHM, target: T, initializer: T, func: (T, T) => T) {
      (bhm /:\ (initializer)(func)) should equal (target)
    }
    val (aInit, aFunc, aRes) = (1,  (a1: A, a2: A) =>  a1 * a2, biHash map (_._1) product)
    val (bInit, bFunc, bRes) = ("", (b1: B, b2: B) => (b1 + b2).sorted, biHash.map(_._2).mkString.sorted)
    forwards (biHash.clone(), (aRes, bRes), (aInit, bInit), { case (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2)) })
    backwards(biHash.clone(), (bRes, aRes), (bInit, aInit), { case (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2)) })
  }

  //@
  //@ Can share with all folds
  //@ Refactor to have this and `foldRight` pointing at the same test code
  // Cryptic symbol for "foldRight"
  test(""":\""") {
    def forwards(bhm: BHM, target: A, initializer: A, func: ((A, B), A) => A) {
      testFunc(bhm, target, initializer, func)
    }
    def backwards(bhm: BHM, target: B, initializer: B, func: ((B, A), B) => B) {
      testFunc(bhm, target, initializer, func)
    }
    def testFunc[T, U](bhm: BHM, target: T, initializer: T, func: ((U, T), T) => T) {
      (bhm :\ (initializer)(func)) should equal (target)
    }
    forwards (biHash.clone(), biHash map (_._1) product, 1,   { case (elem: (Int, String), acc) => acc * elem._1 })
    backwards(biHash.clone(), biHash map (_._2) mkString, "", { case (elem: (String, Int), acc) => acc + elem._1 })
  }

  //@ Can share with `bIterator`
  test("aIterator") {
    (biHash.aIterator.toList sameElements baseList.map(_._1)) should equal (true)
  }

  //@ Can refactor
  test("addString(sb, sep, start, end)") {
    val (sep, start, end) = ("york", "dork", "bjork")
    val target1 = baseList.addString(new StringBuilder()).toString replaceAll (",", " -> ") replaceAll ("\(|\)", "")
    val target2 = baseList.addString(new StringBuilder(), sep).toString replaceAll (",", " -> ") replaceAll ("\(|\)", "")
    val target4 = baseList.addString(new StringBuilder(), sep, start, end).toString replaceAll (",", " -> ") replaceAll ("\(|\)", "")
    biHash.addString(new StringBuilder()).toString should equal (target1)                    // One-arg version
    biHash.addString(new StringBuilder(), sep).toString should equal (target2)               // Two-arg version
    biHash.addString(new StringBuilder(), sep, start, end).toString should equal (target4)   // Four-arg version
  }

  //@
  //@ Can share code with the folds
  test("aggregate(b)(seqop, combop)") {
    def forwards(bhm: BHM, target: A, initializer: A, seqFunc: (A, (A, B)) => A, comboFunc: (A, A) => A) {
      testFunc(bhm, target, initializer, seqFunc, comboFunc)
    }
    def backwards(bhm: BHM, target: B, initializer: B, seqFunc: (B, (B, A)) => B, comboFunc: (B, B) => B) {
      testFunc(bhm, target, initializer, seqFunc, comboFunc)
    }
    def testFunc[T, U](bhm: BHM, target: T, initializer: T, seqFunc: (T, U) => T, comboFunc: (T, T) => T) {
      (bhm aggregate (initializer)(seqFunc, comboFunc)) should equal (target)
    }
    forwards (biHash.clone(), biHash map (_._1) product, 1,   { case (acc, elem: (Int, String)) => acc * elem._1 }, { case (a1: A, a2: A) => -1 })
    backwards(biHash.clone(), biHash map (_._2) mkString, "", { case (acc, elem: (String, Int)) => acc + elem._1 }, { case (b1: B, b2: B) => null })
  }

  test("andThen(func)") {
    biHash andThen (_(baseList(0)._1)) should equal (baseList(0)._2)
  }

  //@
  test("apply(elem)") {
    biHash(baseList(0)._1) should equal (baseList(0)._2)
    biHash(baseList(1)._2) should equal (baseList(1)._1)
  }

  //@ Can share with `bSet`
  test("aSet") {
    (biHash.aSet sameElements baseList.map(_._1)) should equal (true)
  }

  test("bIterator") {
    biHash.aIterator.toList should have ('sameElements (baseList.map(_._1)))
  }

  test("bSet") {
    biHash.aSet should have ('sameElements (baseList.map(_._1)))
  }

  test("clear()") {
    biHash.clear() should equal (BiHashMap[A, B]())
  }

  test("clone()") {
    biHash.clone() should equal (biHash)
  }

  //@
  // Can share below
  // Refactor
  test("collect(pf)") {

    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val aComparator = (a: A) => a < aAverage
    biHash collect { case (a: A, b: B) if (a == null) => b }           should equal (List())                                      // Match (none)
    biHash collect { case (a: A, b: B) if (a == abList.head._1) => b } should equal (List(abList.head._2))                        // Match (one)
    biHash collect { case (a: A, b: B) if (aComparator(a)) => b }      should equal (List(abList filter (aComparator(_._1)): _*)) // Match (some)

    val baList = baseList map (_.swap)
    val bAverage = (baList map (_._1.size) sum) / baList.size
    val bComparator = (b: B) => b.length < bAverage
    biHash collect { case (b: B, a: A) if (b == null) => a }           should equal (List())                                      // Match (none)
    biHash collect { case (b: B, a: A) if (b == baList.head._1) => a } should equal (List(baList.head._2))                        // Match (one)
    biHash collect { case (b: B, a: A) if (bComparator(b)) => a }      should equal (List(baList filter (bComparator(_._1)): _*)) // Match (some)

  }

  //@
  test("collectFirst(pf)") {

    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val aComparator = (a: A) => a < aAverage
    biHash collectFirst { case (a: A, b: B) if (a == null) => b }           should equal (None)                                         // Match (none)
    biHash collectFirst { case (a: A, b: B) if (a == abList.head._1) => b } should equal (Some(abList.head._2))                         // Match (one)
    biHash collectFirst { case (a: A, b: B) if (aComparator(a)) => b }      should equal (Some(abList filter (aComparator(_._1)) head)) // Match (some)

    val baList = baseList map (_.swap)
    val bAverage = (baList map (_._1.size) sum) / baList.size
    val bComparator = (b: B) => b.length < bAverage
    biHash collectFirst { case (b: B, a: A) if (b == null) => a }           should equal (None)                                         // Match (none)
    biHash collectFirst { case (b: B, a: A) if (b == baList.head._1) => a } should equal (Some(baList.head._2))                         // Match (one)
    biHash collectFirst { case (b: B, a: A) if (bComparator(b)) => a }      should equal (Some(baList filter (bComparator(_._1)) head)) // Match (some)

  }

  //@
  // Refactor
  test("compose(func)") {

    val aFunc = (a: A) => a + 1
    intercept[NoSuchElementException] { biHash compose (aFunc(baseList.minBy(_._1)._1)) }
    biHash compose (aFunc(baseList.minBy(_._1)._1 - 1)) should equal (baseList.minBy(_._1)._2)
    
    val bFunc = (b: B) => new String(b.getBytes map (x => (x ^ 2).toByte))
    intercept[NoSuchElementException] { biHash compose (bFunc(baseList.head._2)) }
    biHash compose (bFunc(bFunc(baseList.head._2))) should equal (baseList.head._1)

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
    val arr = new Array[(A, B)](biHash.size)
    biHash.copyToArray(arr)
    arr should equal (biHash.toArray)
  }

  test("copyToArray(arr, start)") {
    val start = 2
    val arr = new Array[(A, B)](biHash.size - start)
    biHash.copyToArray(arr, start)
    arr should equal (biHash.toArray drop (start))
  }

  test("copyToArray(arr, start, len)") {
    val (start, len) = (2, 2)
    val arr = new Array[(A, B)](len)
    biHash.copyToArray(arr, start, len)
    arr should equal (biHash.toArray slice (start, start + len))
  }

  test("copyToBuffer(buff)") {
    val buff = new ListBuffer[(A, B)]()
    biHash.copyToBuffer(buff)
    buff.toList should equal (biHash.toList)
  }

  //@
  // Refactor
  test("count(func)") {

    val abFunc = (ab: (A, B), a: A) => ab._1 < a
    val abList = baseList
    abList map (_._1) foreach (x => biHash count (abFunc(_, x)) should equal (abList count (abFunc(_, x))))

    val baFunc = (ba: (B, A), b: B) => ba._1 < b
    val baList = baseList map (_.swap)
    baList map (_._1) foreach (x => biHash count (baFunc(_, x)) should equal (baList count (baFunc(_, x))))

  }

  //@
  // Refactor
  test("default(elem)") {

    val aDef = 4879
    intercept [NoSuchElementException] { biHash.default(aDef) }

    val bDef = "alksdalsdk"
    intercept [NoSuchElementException] { biHash.default(bDef) }

  }

  // Should share code with the two below
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

    val baCleansed = biHash dropWhile (_._1.contains(""))
    baCleansed.size should equal (0)

    var counter = 0
    val cleansed = biHash takeWhile { case _ => counter += 1; counter < baseList.size }
    cleansed.size should equal (1)

  }

  test("empty") {
    biHash.empty should equal (BiHashMap[A, B]())
  }

  // Can share code with below
  test("ensuring(cond)") {
    intercept [AssertionError] { biHash ensuring ({false}) }
    biHash ensuring ({true}) should equal (biHash)
  }

  test("ensuring(bln)") {
    intercept [AssertionError] { biHash ensuring (false) }
    biHash ensuring (true) should equal (biHash)
  }

  test("equals(any)") {
    val elem = 1001 -> "herpy derpy"
    biHash should equal (biHash)
    biHash should not equal (biHash += elem)
    (biHash += elem) should equal (biHash += elem)
  }

  //@
  // Refactor
  test("exists(func)") {

    val badA = 3421
    val badB = "I'm bad!  Let's go eat some cheeseburgers with President Ronnie!"

    biHash.exists(_ == badA) should equal (false)
    biHash.exists(_ == badB) should equal (false)

    biHash.exists(_ == baseList.head._1) should equal (true)
    biHash.exists(_ == baseList.head._2) should equal (true)

  }

  //@
  // Refactor
  // Share code with some below
  test("filter(func)") {

    val badA = 3421
    val badB = "I'm bad!  Let's go eat some cheeseburgers with President Ronnie!"

    biHash filter { case (a: A, b: B) => a == badA } should equal (biHash)
    biHash filter { case (b: B, a: A) => b == badB } should equal (biHash)

    biHash filter { case (a: A, b: B) => a == baseList.head._1 } should equal (BiHashMap(baseList.head))
    biHash filter { case (b: B, a: A) => b == baseList.head._2 } should equal (BiHashMap(baseList.head))

  }

  // Refactor
  // Share with below
  test("filterAs(func)") {
    biHash filterAs (a => baseList map (_._2) contains (a)) should equal (biHash.empty)
    biHash filterAs (a => baseList map (_._1) contains (a)) should equal (biHash)
    biHash filterAs (_ == baseList.head._1) should equal (BiHashMap(baseList.head))
    biHash filterAs (_ != baseList.tail.head._1) should equal (BiHashMap((baseList.head :: baseList.tail.tail): _*))
  }

  // Refactor
  test("filterBs(func)") {
    biHash filterBs (b => baseList map (_._1) contains (b)) should equal (biHash.empty)
    biHash filterBs (b => baseList map (_._2) contains (b)) should equal (biHash)
    biHash filterBs (_ == baseList.head._2) should equal (BiHashMap(baseList.head))
    biHash filterBs (_ != baseList.tail.head._2) should equal (BiHashMap((baseList.head :: baseList.tail.tail): _*))
  }

  //@
  // Refactor
  test("filterNot(func)") {

    val badA = 3421
    val badB = "I'm bad!  Let's go eat some cheeseburgers with President Ronnie!"

    biHash filter { case (a: A, b: B) => a != badA } should equal (biHash)
    biHash filter { case (b: B, a: A) => b != badB } should equal (biHash)

    biHash filter { case (a: A, b: B) => a != baseList.head._1 } should equal (BiHashMap(baseList.head))
    biHash filter { case (b: B, a: A) => b != baseList.head._2 } should equal (BiHashMap(baseList.head))

  }

  //@
  // Refactor
  // Share code with above
  test("find(func)") {

    val badA = 3421
    val badB = "I'm bad!  Let's go eat some cheeseburgers with President Ronnie!"

    biHash find { case (a: A, b: B) => a != badA } should equal (None)
    biHash find { case (b: B, a: A) => b != badB } should equal (None)

    biHash find { case (a: A, b: B) => a != baseList.head._1 } should equal (Some(baseList.head))
    biHash find { case (b: B, a: A) => b != baseList.head._2 } should equal (Some(baseList.head))

  }

  //@
  // Share code with `map`
  test("flatMap(func)") {

    val abFunc = (ab: (A, B)) => BiHashMap((ab._1 * 6, ab._2))
    (biHash flatMap abFunc) should equal (BiHashMap((baseList map abFunc): _*))

    val baFunc = (ba: (B, A)) => BiHashMap((ba._1, ba._2 * 6))
    (biHash flatMap baFunc) should equal (BiHashMap((baseList map (_.swap) map baFunc): _*))
    
  }

  test("flip") {
    biHash.flip should equal (BiHashMap(baseList map (_.swap): _*))
  }

  //@
  test("fold(res)(func)") {

    def forwards(bhm: BHM, target: (A, B), initializer: (A, B), func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, initializer, func)
    }
    
    def backwards(bhm: BHM, target: (B, A), initializer: (B, A), func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, initializer, func)
    }

    def testFunc[T](bhm: BHM, target: T, initializer: T, func: (T, T) => T) {
      (bhm fold (initializer)(func)) should equal (target)
    }

    val (aInit, aFunc, aRes) = (1,  (a1: A, a2: A) =>  a1 * a2, biHash map (_._1) product)
    val (bInit, bFunc, bRes) = ("", (b1: B, b2: B) => (b1 + b2).sorted, biHash.map(_._2).mkString.sorted)

    forwards (biHash.clone(), (aRes, bRes), (aInit, bInit), { case (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2)) })
    backwards(biHash.clone(), (bRes, aRes), (bInit, aInit), { case (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2)) })

  }

  //@
  test("foldLeft(res)(func)") {

    def forwards(bhm: BHM, target: A, initializer: A, func: (A, (A, B)) => A) {
      testFunc(bhm, target, initializer, func)
    }
    def backwards(bhm: BHM, target: B, initializer: B, func: (B, (B, A)) => B) {
      testFunc(bhm, target, initializer, func)
    }
    def testFunc[T, U](bhm: BHM, target: T, initializer: T, func: (T, (T, U)) => T) {
      (bhm foldLeft (initializer)(func)) should equal (target)
    }
    forwards (biHash.clone(), biHash map (_._1) product, 1,   { case (acc, elem: (Int, String)) => acc * elem._1 })
    backwards(biHash.clone(), biHash map (_._2) mkString, "", { case (acc, elem: (String, Int)) => acc + elem._1 })

  }

  //@
  test("foldRight(res)(func)") {
    def forwards(bhm: BHM, target: A, initializer: A, func: ((A, B), A) => A) {
      testFunc(bhm, target, initializer, func)
    }
    def backwards(bhm: BHM, target: B, initializer: B, func: ((B, A), B) => B) {
      testFunc(bhm, target, initializer, func)
    }
    def testFunc[T, U](bhm: BHM, target: T, initializer: T, func: ((U, T), T) => T) {
      (bhm foldRight (initializer)(func)) should equal (target)
    }
    forwards (biHash.clone(), biHash map (_._1) product, 1,   { case (elem: (Int, String), acc) => acc * elem._1 })
    backwards(biHash.clone(), biHash map (_._2) mkString, "", { case (elem: (String, Int), acc) => acc + elem._1 })
    biHash.mapResult
  }

  //@
  // Refactor
  test("forall(func)") {

    biHash.forall { case (k: A, v: B) => baseList.contains((k, v)) } should be (true)
    biHash.forall { case (k: A, v: B) => v == "five" } should be (false)
    biHash.forall { case (k: A, v: B) => v == "lkjhadskjhsadl" } should be (false)

    biHash.forall { case (k: B, v: A) => baseList map (_.swap) contains((k, v)) } should be (true)
    biHash.forall { case (k: B, v: A) => k == "five" } should be (false)
    biHash.forall { case (k: B, v: A) => k == "lkjhadskjhsadl" } should be (false)

  }

  //@
  // Refactor
  test("foreach(func)") {

    val accInit = (0, "")
    var acc     = accInit
    val aFunc   = (a: A) => acc._1 + (a * 2)
    val bFunc   = (b: B) => acc._2 + b + "!"
    val abFunc  = (ab: (A, B)) => (aFunc(ab._1), bFunc(ab._2))

    biHash foreach { case (a: A, b: B) => acc = abFunc((a, b)) }
    acc should equal ()

    acc = accInit
    biHash foreach { case (b: B, a: A) => acc = abFunc((a, b)) }

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
  // Refactor
  // Can share code with below
  test("getOrElse(a, default)") {

    val badA = 98384
    val badB = "MY EMPORER!  I'VE FAAAAAAILED YOOOOOOU!"

    biHash getOrElse (baseList(2)._1, badB) should equal (baseList(2)._2)
    biHash getOrElse (badA, badB) should equal (badB)

    biHash getOrElse (baseList(2)._2, badA) should equal (baseList(2)._1)
    biHash getOrElse (badB, badA) should equal (badA)

  }

  //@
  // Refactor
  test("getOrElseUpdate(a, => b)") {

    val badA = 98384
    val badB = "MY EMPORER!  I'VE FAAAAAAILED YOOOOOOU!"

    biHash getOrElseUpdate (baseList(2)._1, badB) should equal (baseList(2)._2)
    biHash getOrElseUpdate (badA, badB) should equal (badB)

    biHash getOrElseUpdate (baseList(2)._2, badA) should equal (baseList(2)._1)
    biHash getOrElseUpdate (badB, 49994) should equal (badA)  // A prior line of test code puts `badB <-> badA` into the map

  }

  //@
  // Refactor
  test("groupBy(f)") {

    val newKV = baseList.head._1 * 1000 -> baseList.head._2
    biHash += newKV

    biHash groupBy { case (a: A, b: B) => b } should equal (Map(BiHashMap(baseList.head, newKV), baseList.tail map (BiHashMap(_): _*)))
    biHash groupBy { case (b: B, a: A) => b } should equal (Map(BiHashMap(baseList.head.swap, newKV.swap), baseList.tail map (x => BiHashMap(x.swap): _*)))

  }

  test("grouped(int)") {
    intercept[IllegalArgumentException] { biHash grouped (0) }
    (biHash grouped (1) toList) zip baseList foreach (_ should equal (BiHashMap(_)))
    (biHash grouped (2) toList) zip (baseList grouped (2) toList) foreach { case (bhg, blg) => bhg should equal (BiHashMap(blg: _*)) }
  }

  test("hasDefiniteSize") {
    biHash.hasDefiniteSize should equal (true)
  }

  // Share code with below
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

  // Share code with below
  test("aSet") {
    biHash.aSet should equal (baseList map (_._1) toSet)
  }

  test("bSet") {
    biHash.bSet should equal (baseList map (_._2) toSet)
  }

  // Share code with below
  test("last") {
    baseList should have ('exists (biHash.last))
  }

  test("lastOption") {
    baseList should have ('exists (biHash.lastOption.get))
  }

  //@
  // Refactor
  test("map(func)") {

    val abFunc = (ab: (A, B)) => (ab._1 * 6, ab._2)
    (biHash map abFunc) should equal (BiHashMap((baseList map abFunc): _*))

    val baFunc = (ba: (B, A)) => (ba._1, ba._2 * 6)
    (biHash map baFunc) should equal (BiHashMap((baseList map (_.swap) map baFunc): _*))

  }

  // Refactor
  // Share code with below
  test("mapAs") {
    biHash mapAs (_.toDouble) should have ('sameElements (biHash map { case (a: A, b: B) => (a.toDouble, b) } ))
    biHash.empty mapAs (_.toDouble) should equal (biHash.empty)
  }

  test("mapBs") {
    biHash mapBs (_.getBytes) should have ('sameElements (biHash map { case (a: A, b: B) => (a, b.getBytes) } ))
    biHash.empty mapBs (_.getBytes) should equal (biHash.empty)
  }

  //@
  // Refactor
  test("mapResult") {

    val abFunc = (ab: (A, B)) => (ab._1, ab._2.getBytes)
    biHash.mapResult(_ map abFunc).result() should equal (BiHashMap(baseList map abFunc: _*))

    val baFunc = (ba: (B, A)) => (ba._1, ba._2.toDouble)
    biHash.mapResult(_ map baFunc).result() should equal (BiHashMap(baseList map (_.swap) map baFunc: _*))

  }

  //@
  //@ Can be merged with `minBy`
  test("maxBy") {
    biHash maxBy { case (a: A, b: B) => a } should equal (baseList maxBy (_._1))
    biHash maxBy { case (b: B, a: A) => b.length } should equal (baseList maxBy (_._2.length))
  }

  //@
  test("minBy") {
    biHash minBy { case (a: A, b: B) => a } should equal (baseList.minBy(_._1))
    biHash minBy { case (b: B, a: A) => b.length } should equal (baseList.minBy(_._2.length))
  }

  // All three variants
  //@ Can refactor
  //@ Can share code with `addString`
  test("mkString") {
    val (sep, start, end) = ("york", "dork", "bjork")
    val target0 = baseList.mkString
    val target1 = baseList.mkString(sep)
    val target3 = baseList.mkString(sep, start, end) replaceAll (",", " -> ") replaceAll ("\(|\)", "")
    biHash.mkString should equal (target0)                    // No-arg version
    biHash.mkString(sep) should equal (target1)               // One-arg version
    biHash.mkString(sep, start, end) should equal (target3)   // Three-arg version
  }

  test("nonEmpty") {
    biHash should be ('nonEmpty)
    BiHashMap[A, B]() should not be ('nonEmpty)
  }

  //@
  // Refactor
  test("orElse(pFunc)") {

    val errorResB = "NOOOOOOO!"
    biHash.orElse(errorResB)(baseList.head._1) should equal (baseList.head._2)
    biHash.orElse(errorResB)(1543289513) should equal (errorResB)

    val errorResA = 123123123
    biHash.orElse(errorResA)(baseList.head._2) should equal (baseList.head._1)
    biHash.orElse(errorResA)("alksjdfl;kasjflkasdj") should equal (errorResA)

  }

  //@
  // Refactor
  test("partition(func)") {

    val abList = baseList
    val aAverage = (abList map (_._1) sum) / abList.size
    val abComparator = (ab: (A, B)) => (ab._1 < aAverage)
    biHash.partition(_ == null)              should equal ((biHash.empty, biHash))                                                                        // Match (none, all)
    biHash.partition(_._1 == abList.head._1) should equal ((BiHashMap(abList.head), BiHashMap(abList.tail: _*)))                                          // Match (one, rest)
    biHash.partition(abComparator)           should equal ((BiHashMap(abList filter (abComparator): _*), BiHashMap(abList filterNot (abComparator): _*))) // Match (some, others)

    val baList = baseList map (_.swap)
    val bAverage = (baList map (_._1.size) sum) / baList.size
    val baComparator = (ba: (B, A)) => (ba._1.length < bAverage)
    biHash.partition(_ == null)              should equal ((biHash.empty, biHash))                                                                        // Match (none, all)
    biHash.partition(_._1 == baList.head._1) should equal ((BiHashMap(baList.head), BiHashMap(baList.tail: _*)))                                          // Match (one, rest)
    biHash.partition(baComparator)           should equal ((BiHashMap(baList filter (baComparator): _*), BiHashMap(baList filterNot (baComparator): _*))) // Match (some, others)

  }

  //@
  //@ Can probably share code with the += stuff
  test("put(elem)") {
    def forwards(bhm: BHM, target: BHM, elem: (A, B)) {
      testFunc(bhm, target, elem)
    }
    def backwards(bhm: BHM, target: BHM, elem: (B, A)) {
      testFunc(bhm, target, elem)
    }
    def testFunc[T, U](bhm: BHM, target: BHM, elem: (T, U)) {
      (bhm put elem).get should equal (elem._2)
      bhm should equal (target)
    }
    val myElem = 9001 -> "was?! mein thousand?!!?!?!?!"
    forwards (biHash.clone(), BiHashMap(myElem :: baseList: _*), myElem)
    backwards(biHash.clone(), BiHashMap(myElem :: baseList: _*), myElem.swap)
  }

  //@
  test("reduce(func)") {

    def forwards(bhm: BHM, target: (A, B), func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, func)
    }
    def backwards(bhm: BHM, target: (B, A), func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, func)
    }
    def testFunc[T](bhm: BHM, target: T, func: (T, T) => T) {
      (bhm reduce (func)) should equal (target)
    }
    val (aFunc, aRes) = ((a1: A, a2: A) =>  a1 * a2, biHash map (_._1) product)
    val (bFunc, bRes) = ((b1: B, b2: B) => (b1 + b2).sorted, biHash.map(_._2).mkString.sorted)
    forwards (biHash.clone(), (aRes, bRes), { case (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2)) })
    backwards(biHash.clone(), (bRes, aRes), { case (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2)) })

  }

  //@
  test("reduceLeft(func)") {
    
    def forwards(bhm: BHM, target: (A, B), func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, func)
    }

    def backwards(bhm: BHM, target: (B, A), func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, func)
    }

    def testFunc[T, U](bhm: BHM, target: T, func: (T, T) => T) {
      (bhm reduceLeft (func)) should equal (target)
    }

    val (aFunc, aRes) = ((a1: A, a2: A) => a1 * a2, biHash map (_._1) product)
    val (bFunc, bRes) = ((b1: B, b2: B) => b1 + b2, biHash map (_._2) mkString)
    val abFunc = (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2))
    val baFunc = (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2))

    forwards (biHash.clone(), (aRes, bRes), abFunc)
    backwards(biHash.clone(), (bRes, aRes), baFunc)
    
  }

  //@
  // Share with `reduceLeft`
  test("reduceLeftOption(func)") {

    def forwards(bhm: BHM, target: Option[(A, B)], func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, func)
    }

    def backwards(bhm: BHM, target: Option[(B, A)], func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, func)
    }

    def testFunc[T, U](bhm: BHM, target: Option[T], func: (T, T) => T) {
      (bhm reduceLeftOption (func)) should equal (target)
    }

    val (aFunc, aRes) = ((a1: A, a2: A) => a1 * a2, biHash map (_._1) product)
    val (bFunc, bRes) = ((b1: B, b2: B) => b1 + b2, biHash map (_._2) mkString)
    val abFunc = (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2))
    val baFunc = (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2))

    forwards (biHash.clone(), Option(aRes, bRes), abFunc)
    forwards (biHash.empty,   None,               abFunc)
    backwards(biHash.clone(), Option(bRes, aRes), baFunc)
    backwards(biHash.empty,   None,               baFunc)

  }

  //@
  // Share code with `reduce`
  test("reduceOption(func)") {

    def forwards(bhm: BHM, target: Option[(A, B)], func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, func)
    }

    def backwards(bhm: BHM, target: Option[(B, A)], func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, func)
    }
    
    def testFunc[T](bhm: BHM, target: Option[T], func: (T, T) => T) {
      (bhm reduceOption (func)) should equal (target)
    }

    val (aFunc, aRes) = ((a1: A, a2: A) =>  a1 * a2, biHash map (_._1) product)
    val (bFunc, bRes) = ((b1: B, b2: B) => (b1 + b2).sorted, biHash.map(_._2).mkString.sorted)
    val abFunc = (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2))
    val baFunc = (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2))

    forwards (biHash.clone(), Option(aRes, bRes), abFunc)
    forwards (biHash.empty,   None,               abFunc)
    backwards(biHash.clone(), Option(bRes, aRes), baFunc)
    backwards(biHash.empty,   None,               baFunc)

  }

  //@
  test("reduceRight(func)") {

    def forwards(bhm: BHM, target: (A, B), func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, func)
    }

    def backwards(bhm: BHM, target: (B, A), func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, func)
    }

    def testFunc[T, U](bhm: BHM, target: T, func: (T, T) => T) {
      (bhm reduceRight (func)) should equal (target)
    }

    val (aFunc, aRes) = ((a1: A, a2: A) => a1 * a2, biHash map (_._1) product)
    val (bFunc, bRes) = ((b1: B, b2: B) => b1 + b2, biHash map (_._2) mkString)
    val abFunc = (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2))
    val baFunc = (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2))

    forwards (biHash.clone(), (aRes, bRes), abFunc)
    backwards(biHash.clone(), (bRes, aRes), baFunc)
    
  }

  //@
  test("reduceRightOption(func)") {

    def forwards(bhm: BHM, target: Option[(A, B)], func: ((A, B), (A, B)) => (A, B)) {
      testFunc(bhm, target, func)
    }

    def backwards(bhm: BHM, target: Option[(B, A)], func: ((B, A), (B, A)) => (B, A)) {
      testFunc(bhm, target, func)
    }

    def testFunc[T, U](bhm: BHM, target: Option[T], func: (T, T) => T) {
      (bhm reduceRight (func)) should equal (target)
    }

    val (aFunc, aRes) = ((a1: A, a2: A) => a1 * a2, biHash map (_._1) product)
    val (bFunc, bRes) = ((b1: B, b2: B) => b1 + b2, biHash map (_._2) mkString)
    val abFunc = (acc: (A, B), elem: (A, B)) => (aFunc(acc._1, elem._1), bFunc(acc._2, elem._2))
    val baFunc = (acc: (B, A), elem: (B, A)) => (bFunc(acc._1, elem._1), aFunc(acc._2, elem._2))

    forwards (biHash.clone(), Option(aRes, bRes), abFunc)
    forwards (biHash.empty,   None,               abFunc)
    backwards(biHash.clone(), Option(bRes, aRes), baFunc)
    backwards(biHash.empty,   None,               baFunc)

  }

  //@
  //@ Refactor
  test("remove(elem)") {
    biHash(baseList(0)._1) should equal (baseList(0)._2)
    biHash(baseList(1)._2) should equal (baseList(1)._1)
    biHash.remove(baseList(0)._1).get(baseList(0)._1) should equal (None)
    biHash.remove(baseList(1)._2).get(baseList(1)._2) should equal (None)
  }

  //@
  // Refactor within self
  test("retain(func)") {

    val headMapAB = biHash.retain((a: A, b: B) => (a, b) == biHash.head)
    headMapAB should have ('sameElements (BiHashMap(biHash.head)))

    val tailMapAB = biHash.retain{(a: A, b: B) => val x = (a, b); biHash.tail.exists(_ == x)}  // WHY IS THIS NECESSARY?!
    tailMapAB should have ('sameElements (BiHashMap(biHash.tail: _*)))

    val allMapAB = biHash.retain((a: A, b: B) => true)
    allMapAB should have ('sameElements (biHash))

    val headMapBA = biHash.retain((b: B, a: A) => (b, a) == biHash.head.swap)
    headMapBA should have ('sameElements (BiHashMap(biHash.head.swap)))

    val tailMapBA = biHash.retain{(b: B, a: A) => val x = (b, a); biHash.tail.map(_.swap).exists(_ == x)}  // WHY IS THIS NECESSARY?!
    tailMapBA should have ('sameElements (BiHashMap(biHash.tail.map(_.swap): _*)))

    val allMapBA = biHash.retain((b: B, a: A) => true)
    allMapBA should have ('sameElements (biHash))
    
  }

  //@ Be smart when implementing this!
  test("sameElements(that)") {
    biHash should have ('sameElements (baseList))
    biHash should not have ('sameElements (biHash.empty))
    biHash.empty should have ('sameElements (BiHashMap[A, B]()))
  }

  //@
  // Share code with the two below
  test("scan(res)(func)") {

    val (abBase, abFunc) = ((0, ""), (x: (A, B), y: (A, B)) => (x._1 + y._1, x._2 + y._2))
    biHash.scan(abBase)(abFunc) should have ('sameElements (baseList.scan(abBase)(abFunc)))

    val (baBase, baFunc) = (("", 0), (x: (B, A), y: (B, A)) => (x._1 + y._1, x._2 + y._2))
    biHash.scan(baBase)(baFunc) should have ('sameElements (baseList.map(_.swap).scan(baBase)(baFunc)))

  }

  //@
  test("scanLeft(res)(func)") {

    val (abBase, abFunc) = ((0, ""), (x: (A, B), y: (A, B)) => (x._1 + y._1, x._2 + y._2))
    biHash.scanLeft(abBase)(abFunc) should have ('sameElements (baseList.scanLeft(abBase)(abFunc)))

    val (baBase, baFunc) = (("", 0), (x: (B, A), y: (B, A)) => (x._1 + y._1, x._2 + y._2))
    biHash.scanLeft(baBase)(baFunc) should have ('sameElements (baseList.map(_.swap).scanLeft(baBase)(baFunc)))

  }

  //@
  test("scanRight(res)(func)") {

    val (abBase, abFunc) = ((0, ""), (x: (A, B), y: (A, B)) => (x._1 + y._1, x._2 + y._2))
    biHash.scanRight(abBase)(abFunc) should have ('sameElements (baseList.scanRight(abBase)(abFunc)))

    val (baBase, baFunc) = (("", 0), (x: (B, A), y: (B, A)) => (x._1 + y._1, x._2 + y._2))
    biHash.scanRight(baBase)(baFunc) should have ('sameElements (baseList.map(_.swap).scanRight(baBase)(baFunc)))

  }

  test("size") {
    BiHashMap[A, B]().size should equal (0)
    BiHashMap(baseList(0)).size should equal (1)
    biHash.size should equal (baseList.size)
  }

  test("slice(from, until)") {
    val slice = biHash.slice(1, 4)
    slice should have size (3)
    slice foreach { baseList.contains(_) should equal (true) }
  }

  test("sliding(size)") {
    val slider = biHash.sliding(2)
    val window = slider.next
    window should have size (2)
    window foreach (baseList.contains(_) should equal (true))
  }

  test("sliding(size, step)") {
    val slider = biHash.sliding(2, 2)
    val window = slider.next
    window should have size (2)
    window foreach (baseList.contains(_) should equal (true))
  }

  //@
  // Refactor for code sharing within self
  test("span(func)") {

    val (preAB1, postAB1) = biHash.span(baseList.contains((_: (A, B))))
    preAB1 should have ('sameElements (baseList))
    postAB1 should have size (0)

    val (preAB2, postAB2) = biHash.span(!baseList.contains((_: (A, B))))
    preAB2 should have size (0)
    postAB2 should have ('sameElements (baseList))

    val (preAB3, postAB3) = biHash.span((_: (A, B)) == biHash.head)
    preAB3 should have size (1)
    preAB3.head should equal (biHash.head)
    postAB3 should have size (biHash.size - 1)

    val (preBA1, postBA1) = biHash.span(baseList.contains((_: (B, A))))
    preBA1 should have ('sameElements (baseList))
    postBA1 should have size (0)

    val (preBA2, postBA2) = biHash.span(!baseList.contains((_: (B, A))))
    preBA2 should have size (0)
    postBA2 should have ('sameElements (baseList))

    val (preBA3, postBA3) = biHash.span((_: (B, A)) == biHash.head.swap)
    preBA3 should have size (1)
    preBA3.head should equal (biHash.head)
    postBA3 should have size (biHash.size - 1)

  }

  test("splitAt(n)") {
    val (before, after) = biHash.splitAt(baseList.length / 2)
    before forall (after.get(_._1) should equal (None))
    after forall (before.get(_._1) should equal (None))
    (before.size + after.size) should equal (baseList.size)
    before.size should equal (baseList.length / 2)
    after.size should equal (baseList.length - (baseList.length / 2))
  }

  // Share with `flip`
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

    val baTaken = biHash takeWhile (_._1.contains(""))
    baTaken.toList.distinct.size should equal (baseList.size)
    baTaken.size should equal (baseList.size)

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
    baseList.toIterator should have ('sameElements (biHash.toIterator))
  }

  test("toList") {
    baseList.sameElements(biHash.toList) should equal (true)
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

    val baFunc: (B, A) => A = (k: B, v: A) => k.size + v
    val baTransformed = biHash transform baFunc
    baTransformed should equal (BiHashMap((baseList map (_.swap) map { case (k, v) => (k, (baFunc(k, v))) }): _*))

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
    val aElem = baseList(0)._1 + 987
    val bElem = baseList(0)._2 + "!"
    (biHash updated (baseList(0)._1, bElem))(baseList(0)._1) should equal (bElem)
    (biHash updated (baseList(0)._2, aElem))(baseList(0)._2) should equal (aElem)
  }

  // Refactor
  //@ Is currently inoperatable (must fix return type)
  test("withDefaultA(a)") {
//    val deffy = biHash withDefaultA { case b: B => b.size }
//    val badB  = "jkashdgkjlashdgkljsdagh"
//    deffy(baseList.head._2) should equal (baseList.head._1)
//    deffy(badB) should equal (badB.size)
  }

  // Refactor
  //@ Can share code with above
  test("withDefaultB(b)") {

    val aFunc = (a: A) => a.toString + " is outta this world!"
    val deffy = biHash withDefaultB aFunc
    val badA  = -18

    deffy(baseList.head._1) should equal (baseList.head._2)
    deffy(badA) should equal (aFunc(badA))

  }

  //@
  test("withFilter(func)") {

    import collection.mutable.ListBuffer
    
    val abBuffer = new ListBuffer[(A, B)]()
    biHash withFilter (_._1 < (baseList map (_._1) max)) foreach (abBuffer += _)
    abBuffer.toList.size should equal (baseList.size - 1)

    val baBuffer = new ListBuffer[(B, A)]()
    biHash withFilter (_._1.contains("klajshfgkljadngkljangdkjlangdkljn")) foreach (baBuffer += _)
    baBuffer.toList.size should equal (0)
    
  }

  //@ Can share code with below
  test("zip(that)") {
    val zipped = (biHash zip (0 to baseList.size))
    zipped.size should equal (baseList.size)
    zipped should equal (BiHashMap[(A, B), Int](baseList.zipWithIndex))
  }

  test("zipAll(that)") {
    val elem = 15 -> "Beedrill"
    val zipped = (biHash zipAll (0 to baseList.size, elem, -1))
    zipped.size should equal (baseList.size + 1)
    zipped should not equal (BiHashMap[(A, B), Int](baseList.zipWithIndex))
    zipped should equal (BiHashMap[(A, B), Int]((elem :: baseList).zipWithIndex))
  }

  test("zipWithIndex") {
    val elem = baseList(0)
    BiHashMap[A, B](elem).zipWithIndex.apply(elem) should equal (0)
  }

}
