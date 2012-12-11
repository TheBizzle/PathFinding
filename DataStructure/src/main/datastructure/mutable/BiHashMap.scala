package datastructure.mutable

import collection.{ CustomParallelizable, generic, mutable }
import mutable.HashMap
import generic.{ CanBuildFrom, MutableMapFactory }

import datastructure.parallel.mutable.ParBiHashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/18/12
 * Time: 11:15 PM
 */

//@ Should probably implement `toString` correctly; currently claims this to be a `Map`
//@ Should add `<->` as a tuple-building operator
class BiHashMap[A, B] private[datastructure](override protected val abMap: HashMap[A, B], override protected val baMap: HashMap[B, A])
    extends HashMap[A, B]
    with Bijection[A, B, HashMap, BiHashMap]
    with CustomParallelizable[(A, B), ParBiHashMap[A, B]]
    with BiHashForwardOps[A, B]
    with BiHashReverseOps[A, B] {

  def this(contents: (A, B)*)(implicit aToBMap: HashMap[A, B] = contents.foldRight(new HashMap[A, B]){ case (ab, m) => m += ab },
                                       bToAMap: HashMap[B, A] = contents.foldRight(new HashMap[B, A]){ case (ab, m) => m += ab.swap }) {
    this(aToBMap, bToAMap)
  }

  override def canEqual(other: Any) : Boolean            = other.isInstanceOf[BiHashMap[A, B]]  // Might pay to do "|| other.isInstanceOf[BiHashMap[B, A]]"... if not for type erasure
  override def clone()              : BiHashMap[A, B]    = new BiHashMap(abMap.toSeq: _*)
  override def myEmpty              : BiHashMap[A, B]    = BiHashMap.empty[A, B]
  override def par                  : ParBiHashMap[A, B] = throw new UnsupportedOperationException("`ParBiHashMap` is not yet in an operable state.") //@
  override def swap                 : BiHashMap[B, A]    = new BiHashMap(baMap.clone(), abMap.clone())

  // Toggles whether a size map is used to track hash map statistics for the child maps.
  override def useSizeMap(t: Boolean) {
    abMap.useSizeMap(t)
    baMap.useSizeMap(t)
  }

}

object BiHashMap extends MutableMapFactory[BiHashMap] {
  implicit def canBuildFrom[A, B] : CanBuildFrom[Coll, (A, B), BiHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B] : BiHashMap[A, B] = new BiHashMap[A, B]()
}
