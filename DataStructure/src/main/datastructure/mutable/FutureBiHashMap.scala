package datastructure.mutable

import datastructure.Bijection
import collection.generic.{CanBuildFrom, MutableMapFactory}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/18/12
 * Time: 11:15 PM
 */

class FutureBiHashMap[A, B] private[datastructure](contents: (A, B)*) extends Bijection[A, B](contents: _*) with BiHashForwardOps[A, B] with BiHashReverseOps[A, B] {

  def aValues : scala.collection.Set[A] = abMap.keySet
  def bValues : scala.collection.Set[B] = baMap.keySet
  def size    : Int                     = { require (abMap.size == baMap.size); abMap.size }
  def clear() { abMap.clear(); baMap.clear() }

  def canEqual(other: Any)       : Boolean = other.isInstanceOf[FutureBiHashMap[A, B]]  // Might pay to do "|| other.isInstanceOf[BiHashMap[B, A]]"... if not for type erasure
  override def equals(that: Any) : Boolean = {
    that match {
      case thatHash: FutureBiHashMap[A, B] => (thatHash canEqual this) &&
                                        ( (thatHash.abMap.equals(abMap) && thatHash.baMap.equals(baMap)) ||
                                          (thatHash.abMap.equals(baMap) && thatHash.baMap.equals(abMap)) )
      case _                         => false
    }
  }

  override def clone    : FutureBiHashMap[A, B] = new FutureBiHashMap(abMap.toSeq: _*)
  override def hashCode : Int                   = abMap.hashCode() ^ baMap.hashCode()        // XOR the hashcodes of the two maps

}

object FutureBiHashMap {//extends MutableMapFactory[FutureBiHashMap] {
  def apply[A, B](contents: (A, B)*) : FutureBiHashMap[A, B] = new FutureBiHashMap(contents: _*)
  //implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), FutureBiHashMap[A, B]] = new MapCanBuildFrom[A, B]
  //def empty[A, B]: FutureBiHashMap[A, B] = new FutureBiHashMap[A, B]()
}
