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
    def apply(aKey: => A) : Option[B] = {
        get(aKey)
    }

    def apply(bKey: B) : Option[A] = {
        get(bKey)
    }

    def get(aKey: => A) : Option[B] = {
        abMap.get(aKey)
    }

    def get(bKey: B) : Option[A] = {
        baMap.get(bKey)
    }

    def += (ab: => (A,  B)) : BiHashMap[A, B] = {
        put(ab._1, ab._2)
        this
    }

    def += (ba: (B,  A)) : BiHashMap[A, B] = {
        put(ba._1, ba._2)
        this
    }

    def put(aKey: => A, bVal: => B) : Option[B] = {
        baMap.put(bVal, aKey)
        abMap.put(aKey, bVal)
    }

    def put(bKey: B, aVal: A) : Option[A] = {
        abMap.put(aVal, bKey)
        baMap.put(bKey, aVal)
    }

    def remove(aKey: => A) : Option[B] = {
        abMap.get(aKey) match {
            case Some(b) => baMap.remove(b)
            case None => // Doesn't matter
        }
        abMap.remove(aKey)
    }

    def remove(bKey: B) : Option[A] = {
        baMap.get(bKey) match {
            case Some(a) => abMap.remove(a)
            case None => // Doesn't matter
        }
        baMap.remove(bKey)
    }

    def -= (aKey: => A) : BiHashMap[A, B] = {
        remove(aKey)
        this
    }

    def -= (bKey: B) : BiHashMap[A, B] = {
        remove(bKey)
        this
    }

    override def clone : BiHashMap[A, B] = {
        new BiHashMap(abMap.clone(), baMap.clone())
    }

    def update(aKey: => A, bVal: => B) {

        val hold = abMap.get(aKey)
        abMap.update(aKey, bVal)

        hold match {
            case Some(b) => { baMap.remove(b); baMap.put(bVal, aKey) }
            case None => // Doesn't matter
        }

    }

    def update(bKey: B, aVal: A) {

        val hold = baMap.get(bKey)
        baMap.update(bKey, aVal)

        hold match {
            case Some(a) => { abMap.remove(a); abMap.put(aVal, bKey) }
            case None => // Doesn't matter
        }

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
        if (that.isInstanceOf[BiHashMap[A, B]]) {
            val thatHash = that.asInstanceOf[BiHashMap[A, B]]
            thatHash.abMap.equals(abMap) && thatHash.baMap.equals(baMap)
        }
        else
            false
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
        def applyHelper[A,B] (retMap: BiHashMap[A,B], tupList: List[(A, B)]) : BiHashMap[A, B] = {
            tupList match {
                case (a, b)::t => { retMap.put(a, b); applyHelper(retMap, t) }
                case Nil => retMap
            }
        }
        applyHelper(new BiHashMap[A, B](), tupSeq.seq.toList)
    }
}