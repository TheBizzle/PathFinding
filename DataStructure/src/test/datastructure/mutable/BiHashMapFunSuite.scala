package datastructure.mutable

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import collection.GenTraversableOnce

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:20 PM
 */

// Oh, lawd...
class BiHashMapFunSuite extends FunSuite with BeforeAndAfterEach with ShouldMatchers {

  val aList = List(5, 17, 1, 9, 4)
  val bList = List("five", "seventeen", "one", "nine", "four")
  val inList = aList.zip(bList)
  val biHash = BiHashMap[Int, String]()

  override def beforeEach() {
    biHash.clear()
    biHash ++= inList
    super.beforeEach()
  }
  
  test("==") {
    (biHash == BiHashMap[Double, String]()) should equal (false)
    (biHash == BiHashMap((aList tail) zip (bList tail) map { case (i, s) => (i.toDouble, s) }: _*)) should equal (false)
    (biHash == BiHashMap(inList map { case (i, s) => (i.toDouble, s) }: _*)) should equal (false)
    (biHash == BiHashMap[Int, String]()) should equal (false)
    (biHash == BiHashMap((aList tail) zip (bList tail): _*)) should equal (false)
    (biHash == BiHashMap(inList: _*)) should equal (true)
  }

  test("!=") {
    (biHash != BiHashMap[Double, String]()) should equal (true)
    (biHash != BiHashMap((aList tail) zip (bList tail) map { case (i, s) => (i.toDouble, s) }: _*)) should equal (true)
    (biHash != BiHashMap(inList map { case (i, s) => (i.toDouble, s) }: _*)) should equal (true)
    (biHash != BiHashMap[Int, String]()) should equal (true)
    (biHash != BiHashMap((aList tail) zip (bList tail): _*)) should equal (true)
    (biHash != BiHashMap(inList: _*)) should equal (false)
  }

  //?
  //@
  test("+(elem)") {
    def forwards(bhm: BiHashMap[Int, String], rootList: List[(Int, String)], kvPairs: (Int, String)*) {
      testFunc(bhm, BiHashMap(rootList ++ kvPairs: _*), kvPairs: _*)
    }
    def backwards(bhm: BiHashMap[Int, String], rootList: List[(String, Int)], kvPairs: (String, Int)*) {
      testFunc(bhm, BiHashMap(rootList ++ kvPairs: _*), kvPairs: _*)
    }
    def testFunc[T, U](bhm: BiHashMap[Int, String], target: BiHashMap[T, U], kvPairs: (T, U)*) {
      bhm + kvPairs(0) should equal (target)
      bhm + kvPairs(1) should equal (target)
    }
    val args = Seq(9001 -> "nein tousend won", 4 -> "fier")
    forwards(biHash.clone(), inList, args: _*)
    backwards(biHash.clone(), inList map (_.swap), args map (_.swap): _*)
  }

  //?
  //@
  test("+(elem1, elem2, elems)") {
    def forwards(bhm: BiHashMap[Int, String], rootList: List[(Int, String)], kvPairs: (Int, String)*) {
      testFunc(bhm, BiHashMap(rootList ++ kvPairs: _*), kvPairs: _*)
    }
    def backwards(bhm: BiHashMap[Int, String], rootList: List[(String, Int)], kvPairs: (String, Int)*) {
      testFunc(bhm, BiHashMap(rootList.map (_.swap) ++ kvPairs: _*), kvPairs: _*)
    }
    def testFunc[T, U](bhm: BiHashMap[Int, String], target: BiHashMap[T, U], kvPairs: (T, U)*) {
      bhm + (kvPairs(0), kvPairs(1), kvPairs.splitAt(2)._2: _*) should equal (target)
    }
    val args = Seq(9001 -> "nein tousend won", 4 -> "fier", 3 -> "shree, akchurry")
    forwards(biHash.clone(), inList, args: _*)
    backwards(biHash.clone(), inList map (_.swap), args map (_.swap): _*)
  }

  //?
  //@
  test("++(that)") {
    def forwards(bhm: BiHashMap[Int, String], appendee: GenTraversableOnce[(Int, String)], target: GenTraversableOnce[(Int, String)]) {
      testFunc(bhm, bhm, bhm)
      testFunc(bhm, appendee, target)
    }
    def backwards(bhm: BiHashMap[Int, String], appendee: GenTraversableOnce[(String, Int)], target: GenTraversableOnce[(String, Int)]) {
      testFunc(bhm, bhm.flip, bhm)
      testFunc(bhm, appendee, target)
    }
    def testFunc[T, U](bhm: BiHashMap[Int, String], appendee: GenTraversableOnce[(T, U)], target: GenTraversableOnce[(T, U)]) {
      bhm ++ appendee should equal (target)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards(biHash.clone(), myList, List(biHash.toSeq ++ myList: _*))
    backwards(biHash.clone(), myList map (_.swap), List((biHash.toSeq ++ myList) map (_.swap): _*))
  }

  //?
  //@
  test("++:(that)") {
    def forwards(bhm: BiHashMap[Int, String], appendee: Traversable[(Int, String)], target: Traversable[(Int, String)]) {
      testFunc(bhm, bhm, bhm)
      testFunc(bhm, appendee, target)
    }
    def backwards(bhm: BiHashMap[Int, String], appendee: Traversable[(String, Int)], target: Traversable[(String, Int)]) {
      testFunc(bhm, bhm.flip, bhm)
      testFunc(bhm, appendee, target)
    }
    def testFunc[T, U](bhm: BiHashMap[Int, String], appendee: Traversable[(T, U)], target: Traversable[(T, U)]) {
      bhm ++: appendee should equal (target)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards(biHash.clone(), myList, List(biHash.toSeq ++ myList: _*))
    backwards(biHash.clone(), myList map (_.swap), List((biHash.toSeq ++ myList) map (_.swap): _*))
  }

  //?
  //@
  test("++=(traversableonce)") {
    def forwards(bhm: BiHashMap[Int, String], appendee: TraversableOnce[(Int, String)], target: TraversableOnce[(Int, String)]) {
      testFunc(bhm, appendee, target)
    }
    def backwards(bhm: BiHashMap[Int, String], appendee: TraversableOnce[(String, Int)], target: TraversableOnce[(String, Int)]) {
       testFunc(bhm, appendee, target)
    }
    def testFunc[T, U](bhm: BiHashMap[Int, String], appendee: TraversableOnce[(T, U)], target: TraversableOnce[(T, U)]) {
      val orig = bhm.clone(); bhm ++= bhm; orig should equal (bhm)
      bhm ++= appendee; bhm should equal (target)
    }
    val myList = List(100 -> "hundred", 1000 -> "thousand", 666 -> "satanry")
    forwards(biHash.clone(), myList, List(biHash.toSeq ++ myList: _*))
    backwards(biHash.clone(), myList map (_.swap), List((biHash.toSeq ++ myList) map (_.swap): _*))
  }

  test("+=(entry)") {
    def forwards(bhm: BiHashMap[Int, String], appendee: TraversableOnce[(Int, String)], target: TraversableOnce[(Int, String)]) {
      testFunc(bhm, appendee, target)
    }
    def backwards(bhm: BiHashMap[Int, String], appendee: TraversableOnce[(String, Int)], target: TraversableOnce[(String, Int)]) {
       testFunc(bhm, appendee, target)
    }
    def testFunc[T, U](bhm: BiHashMap[Int, String], appendee: TraversableOnce[(T, U)], target: TraversableOnce[(T, U)]) {
      val orig = bhm.clone(); bhm ++= bhm; orig should equal (bhm)
      bhm ++= appendee; bhm should equal (target)
    }
  }

  test("+=(elem1, elem2, elems)") {
  
  }

  //@
  test("-(A)") {
  
  }

  //@
  test("-(elem1, elem2, elems)") {
  
  }

  //@
  test("--(gentraversableonce[A])") {
  
  }

  //@
  test("--=(traversableonce[A])") {
  
  }

  //@
  test("-=(elem)") {
  
  }

  //@
  test("-=(elem1, elem2, elems)") {
  
  }

  test("->()") {
  
  }

  //@
  // Monkeyface operators...
  test("/:()") {
  
  }

  //@
  test("/:\()") {
  
  }

  //@
  test(":\()") {
  
  }

  test("aIterator") {
  
  }

  test("aValues") {
  
  }

  test("addString(sb)") {
  
  }

  test("addString(sb, sep)") {
  
  }

  test("addString(sb, sep, start, end)") {
  
  }

  test("aggregate(b)(seqop, combop)") {
  
  }

  //@
  test("andThen(func)") {
  
  }

  //@
  test("(elem)") {
  
  }

  test("bIterator") {
  
  }

  test("bValues") {
  
  }

  test("clear()") {
  
  }

  test("clone()") {
  
  }

  //@
  test("collect(pf)") {
  
  }

  //@
  test("collectFirst(pf)") {
  
  }

  //@
  test("compose(fumc)") {
  
  }

  //@
  test("contains(elem)") {
  
  }

  test("copyToArray(arr)") {
  
  }

  test("copyToArray(arr, start)") {
  
  }

  test("copyToArray(arr, start, len)") {
  
  }

  test("copyToBuffer(buff)") {
  
  }

  //@
  test("count(func)") {
  
  }

  //@
  test("default(elem)") {
  
  }

  test("drop(n)") {
  
  }

  test("dropRight(n)") {
  
  }

  //@
  test("dropWhile(n)") {
  
  }

  test("empty") {
  
  }

  //@
  test("ensuring(cond)") {
  
  }

  test("ensuring(cond, msg)") {
  
  }

  test("ensuring(bln)") {
  
  }

  test("ensuring(bln, msg)") {
  
  }

  test("equals(any)") {
  
  }

  //@
  test("exists(func)") {
  
  }

  //@
  test("filter(func)") {
  
  }

  //@
  test("filterKeys(func)") {
  
  }

  //@
  test("filterNot(func)") {
  
  }

  //@
  test("find(func)") {
  
  }

  //@
  test("flatMap(func)") {
  
  }

  //@
  test("flatten(func)") {
  
  }

  //@
  test("fold(res)(func)") {
  
  }

  //@
  test("foldLeft(res)(func)") {
  
  }

  //@
  test("foldRight(res)(func)") {
  
  }

  //@
  test("forall(func)") {
  
  }

  //@
  test("foreach(func)") {
  
  }

  test("formatted(str)") {
  
  }

  //@
  test("get(elem)") {
  
  }

  //@
  test("getOrElse(a, default)") {
  
  }

  //@
  test("getOrElseUpdate(a, => b)") {
  
  }

  //@
  test("groupBy(f)") {
  
  }

  test("grouped(int)") {
  
  }

  test("hasDefiniteSize") {
  
  }

  test("hashCode()") {
  
  }

  test("head") {
  
  }

  test("headOption") {
  
  }

  // Decide
  test("init") {
  
  }

  // Decide
  test("inits") {
  
  }

  //@
  test("isDefinedAt(key)") {
  
  }

  test("isEmpty") {
  
  }

  test("isTraversableAgain") {
  
  }

  test("iterator") {
  
  }

  //@ Morph
  test("keys") {
  }

  test("last") {
  
  }

  test("lastOption") {
  
  }

  //@
  test("lift") {
  
  }

  //@
  test("map(func)") {
  
  }

  //@
  // ??!?!?!?!
  test("mapResult(func)") {
  
  }

  //@
  // Break and override
  test("mapValues") {
  
  }

  //@
  test("max") {
  
  }

  //@
  test("maxBy") {
  
  }

  //@
  test("min") {
  
  }

  //@
  test("minBy") {
  
  }

  // All three variants
  test("mkString") {
  
  }

  test("nonEmpty") {
  
  }

  //@
  test("orElse(pFunc)") {
  
  }

  // Test, regardless of brokenness
  test("par") {
  
  }

  //@
  test("partition(func)") {
  
  }

  //@
  // ?!?!??!?!
  test("product") {
  
  }

  //@
  test("put(elem)") {
  
  }

  //@
  test("reduce(func)") {
  
  }

  //@
  test("reduceLeft(func)") {
  
  }

  //@
  test("reduceLeftOption(func)") {
  
  }

  //@
  test("reduceOption(func)") {
  
  }

  //@
  test("reduceRight(func)") {
  
  }

  //@
  test("reduceRightOption(func)") {
  
  }

  //@
  test("remove(elem)") {
  
  }

  // ?!?!?!??
  test("result()") {
  
  }

  //@
  test("retain(func)") {
  
  }

  // Be smart when implementing this!
  test("sameElements(that)") {
  
  }


  //@
  test("scan(res)(func)") {
  
  }

  //@
  test("scanLeft(res)(func)") {
  
  }

  //@
  test("scanRight(res)(func)") {
  
  }

  // ?!?!? What's up with the return type?
  test("seq") {
  
  }

  test("size") {
  
  }

  test("sizeHint(size)") {
  
  }

  test("sizeHint(coll, delta)") {
  
  }

  test("sizeHintBounded(size, boundingColl)") {
  
  }

  test("slice(from, until)") {
  
  }

  test("sliding(size)") {
  
  }

  test("sliding(size, step)") {
  
  }

  //@
  test("span(func)") {
  
  }

  //@
  test("splitAt(n)") {
  
  }

  test("stringPrefix") {
  
  }

  // ?!?!?!?!
  test("sum") {
  
  }

  // ?!?!?!??
  test("synchronized") {
  
  }

  test("tail") {
  
  }

  test("tails") {
  
  }

  test("take(n)") {
  
  }

  test("takeRight(n)") {
  
  }

  //@
  test("takeWhile(func)") {
  
  }

  test("toArray") {
  
  }

  test("toBuffer") {
  
  }

  test("toIndexedSeq") {
  
  }

  test("toIterable") {
  
  }

  test("toIterator") {
  
  }

  test("toList") {
  
  }

  test("toMap") {
  
  }

  test("toSeq") {
  
  }

  test("toSet") {
  
  }

  test("toStream") {
  
  }

  test("toString()") {
  
  }

  test("toTraversable") {
  
  }

  //@
  test("transform(func)") {
  
  }

  //@
  // ?!?!?!?
  test("transpose") {
  
  }

  //@
  test("unzip") {
  
  }

  //@
  test("unzip3") {
  
  }

  //@
  test("update(key, val)") {
    
  }
  
  //@
  test("updated(a, b)") {
  
  }

  test("useSizeMap(bln)") {
  
  }

  test("view") {
  
  }

  test("view(from, until)") {
  
  }

  //@
  test("withDefault(func)") {
  
  }

  //@ Break and replace with A/Bs
  test("withDefaultValue(alks)") {
  
  }

  //@
  test("withFilter(func)") {
  
  }

  test("zip(that)") {
  
  }

  test("zipAll(that)") {
  
  }

  test("zipWithIndex") {
  
  }


}
