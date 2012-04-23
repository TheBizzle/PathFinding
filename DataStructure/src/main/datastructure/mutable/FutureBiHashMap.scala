package datastructure.mutable

import datastructure.Bijection
import collection.generic.{CanBuildFrom, MutableMapFactory}
import datastructure.parallel.mutable.ParBiHashMap
import collection.mutable.MapLike
import collection.mutable.Map
import collection.CustomParallelizable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/18/12
 * Time: 11:15 PM
 */

class FutureBiHashMap[A, B] private[datastructure](contents: (A, B)*) extends Bijection[A, B](contents: _*)
                                                                      with Map[A, B]
                                                                      with MapLike[A, B, FutureBiHashMap[A, B]]
                                                                      with CustomParallelizable[(A, B), ParBiHashMap[A, B]]
                                                                      with BiHashForwardOps[A, B]
                                                                      with BiHashReverseOps[A, B] {

  // Toggles whether a size map is used to track hash map statistics for the child maps.
  def useSizeMap(t: Boolean) { abMap.useSizeMap(t); baMap.useSizeMap(t) }
  def iterator : Iterator[(A, B)] = abMap.iterator

  override def clone()    : FutureBiHashMap[A, B] =   new FutureBiHashMap(abMap.toSeq: _*)
  override def hashCode() : Int                   =   abMap.hashCode() ^ baMap.hashCode()   // XOR the hashcodes of the two maps
  override def clear()                              { abMap.clear(); baMap.clear() }
  override def size     : Int                     = { require (abMap.size == baMap.size); abMap.size }
  override def par        : ParBiHashMap[A, B]    =   new ParBiHashMap[A, B](contents: _*)  //@ Yeah, ummm... don't use this.  I'm considering just having it throw an exception...
  override def empty      : FutureBiHashMap[A, B] =   FutureBiHashMap.empty[A, B]
  override def canEqual(other: Any) : Boolean = other.isInstanceOf[FutureBiHashMap[A, B]]  // Might pay to do "|| other.isInstanceOf[BiHashMap[B, A]]"... if not for type erasure
  override def equals(that: Any)    : Boolean = {
    that match {
      case thatHash: FutureBiHashMap[A, B] => (thatHash canEqual this) &&
                                        ( (thatHash.abMap.equals(abMap) && thatHash.baMap.equals(baMap)) ||
                                          (thatHash.abMap.equals(baMap) && thatHash.baMap.equals(abMap)) )
      case _                         => false
    }
  }

  @deprecated("Using this will throw an exception!  Use `aValues` or `bValues` instead.", "forever")
  override def keySet : collection.immutable.Set[A] = {
    throw new UnsupportedOperationException("`keySet` function ambiguous for BiHashMap; use `aValues` or `bValues`, instead")
  }

  @deprecated("Using this will throw an exception!  Use `aValues` or `bValues` instead.", "forever")
  override def values : collection.Iterable[B] = {
    throw new UnsupportedOperationException("`values` function ambiguous for BiHashMap; use `aValues` or `bValues`, instead")
  }

}

object FutureBiHashMap extends MutableMapFactory[FutureBiHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), FutureBiHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: FutureBiHashMap[A, B] = new FutureBiHashMap[A, B]()
}
