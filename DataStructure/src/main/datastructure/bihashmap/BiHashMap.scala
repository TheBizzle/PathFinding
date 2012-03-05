package datastructure.bihashmap

import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 4:33 PM
 * Description: INCOMPLETE
 *              EXPERIMENTAL
 *
 *              A wrapper around two HashMaps, which allows for easily and agnostically getting an A from a B or a B from an A.
 *
 *              Does not currently support mappings of A->B where A and B are of the same end type.
 *              Well... it might work TO SOME EXTENT, but it's not very safe or likely to do entirely what you'd want.
 *
 *              If you want to use it with A and B both being the same type (C), I suggest writing a
 *              shallow wrapper class (D), such that D extends/encapsulates C, and then making all things from
 *              exactly one of your C sets into Ds, allowing you to make a BiHashMap[A, D], or a BiHashMap[B, D].
 *              If the above is done, BiHashMap should work properly with your data sets (though with the moderate
 *              inconvenience of wrapping your data and possibly needing to unwrap it).
 */

class BiHashMap[A: Manifest, B: Manifest] protected (aToBMap: HashMap[A, B], bToAMap: HashMap[B, A]) {

    private val abMap = aToBMap
    private val baMap = bToAMap

    def this() {
        this(new HashMap[A, B], new HashMap[B, A])
    }

    // The arrow in the parameter list is a dirty Scala trick for getting around the fact that
    // apply(A) and apply(B) have the same signatures to the JVM after type erasure
    // (Many times, you'll see a by-name parameter of type A get stored into a variable right away;
    //  that's so we don't accidentally keep reevaluating it each time that it is used)
    def apply(aKey: => A) : B = {
        val a = aKey
        abMap(a)
    }

    def apply(bKey: B) : A = {
        baMap(bKey)
    }

    def default (aKey: => A) : B = {
        val a = aKey
        throw new NoSuchElementException("key not found: " + a)
    }

    def default (bKey: B) : A = {
        throw new NoSuchElementException("key not found: " + bKey)
    }

    def get(aKey: => A) : Option[B] = {
        val a = aKey
        abMap.get(a)
    }

    def get(bKey: B) : Option[A] = {
        baMap.get(bKey)
    }

    def += (ab: => (A,  B)) : BiHashMap[A, B] = {
        val abTuple = ab; import abTuple._
        put(_1, _2)
        this
    }

    def += (ba: (B,  A)) : BiHashMap[A, B] = {
        put(ba._1, ba._2)
        this
    }

    def put(aKey: => A, bVal: B) : Option[B] = {
        val a = aKey
        baMap.put(bVal, a)
        abMap.put(a, bVal)
    }

    def put(bKey: B, aVal: A) : Option[A] = {
        abMap.put(aVal, bKey)
        baMap.put(bKey, aVal)
    }

    def remove(aKey: => A) : Option[B] = {
        val a = aKey
        abMap.get(a) foreach (baMap.remove(_))
        abMap.remove(a)
    }

    def remove(bKey: B) : Option[A] = {
        baMap.get(bKey) foreach (abMap.remove(_))
        baMap.remove(bKey)
    }

    def -= (aKey: => A) : BiHashMap[A, B] = {
        val a = aKey
        remove(a)
        this
    }

    def -= (bKey: B) : BiHashMap[A, B] = {
        remove(bKey)
        this
    }

    override def clone : BiHashMap[A, B] = {
        new BiHashMap(abMap.clone(), baMap.clone())
    }

    def update(aKey: => A, bVal: B) {
        val a = aKey
        val hold = abMap.get(a)
        abMap.update(a, bVal)
        hold foreach { b => baMap.remove(b); baMap.put(bVal, a) }
    }

    def update(bKey: B, aVal: A) {
        val hold = baMap.get(bKey)
        baMap.update(bKey, aVal)
        hold foreach { a => abMap.remove(a); abMap.put(aVal, bKey) }
    }

    def clear() {
        abMap.clear()
        baMap.clear()
    }

    def contains(aKey: => A) : Boolean = {
        abMap.contains(aKey)
    }

    def contains(bKey: B) : Boolean = {
        baMap.contains(bKey)
    }

    override def equals(that: Any) : Boolean = {
        that match {
            case thatHash: BiHashMap[A, B] => ((thatHash.abMap.equals(abMap) && thatHash.baMap.equals(baMap)) || (thatHash.abMap.equals(baMap) && thatHash.baMap.equals(abMap)))
            case _                         => false
        }
    }

    override def hashCode : Int = {
        abMap.hashCode() ^ baMap.hashCode()  // XOR the hashcodes of the two maps
    }

    def canEqual(other: Any) : Boolean = {
        other.isInstanceOf[BiHashMap[A, B]] || other.isInstanceOf[BiHashMap[B, A]]
    }

    def ASet : scala.collection.Set[A] = {
        abMap.keySet
    }

    def BSet : scala.collection.Set[B] = {
        baMap.keySet
    }

    def size : Int = {
        abMap.size
    }

}

object BiHashMap {
    def apply[A: Manifest, B: Manifest] (tupSeq: (A, B)*) : BiHashMap[A, B] = {
        val bhMap = new BiHashMap[A, B]()
        tupSeq foreach { case (a, b) => bhMap.put(a, b) }
        bhMap
    }
}
