package datastructure.mutable

import datastructure.Bijection
import collection.generic.{CanBuildFrom, MutableMapFactory}
import datastructure.parallel.mutable.ParBiHashMap
import collection.CustomParallelizable
import collection.mutable.{HashMap, MapLike}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/18/12
 * Time: 11:15 PM
 */

//@ Should I instantiate and fill the maps here, or should I instantiate here and leave the population of the maps to `Bijection`...?
class BiHashMap[A, B] private[datastructure](contents: (A, B)*)
                                            (implicit aToBMap: HashMap[A, B] = contents.foldRight(new HashMap[A, B]){ case (ab, m) => m += ab },
                                                      bToAMap: HashMap[B, A] = contents.foldRight(new HashMap[B, A]){ case (ab, m) => m += ab._2 -> ab._1 })
    extends Bijection(aToBMap, bToAMap)
    with MapLike[A, B, BiHashMap[A, B]]
    with CustomParallelizable[(A, B), ParBiHashMap[A, B]]
    with BiHashForwardOps[A, B]
    with BiHashReverseOps[A, B] {

  def useSizeMap(t: Boolean) { abMap.useSizeMap(t); baMap.useSizeMap(t) }

  override def clone() : BiHashMap[A, B]      = new BiHashMap(abMap.toSeq: _*)
  override def par     : ParBiHashMap[A, B]   = new ParBiHashMap[A, B](contents: _*)  //@ Yeah, ummm... don't use this.  I'm considering just having it throw an exception...
  override def empty   : BiHashMap[A, B]      = BiHashMap.empty[A, B]
  override def canEqual(other: Any) : Boolean = other.isInstanceOf[BiHashMap[A, B]]  // Might pay to do "|| other.isInstanceOf[BiHashMap[B, A]]"... if not for type erasure

  @deprecated("Using this will throw an exception!  Use `aValues` or `bValues` instead.", "forever")
  override def keySet : collection.immutable.Set[A] = {
    throw new UnsupportedOperationException("`keySet` function ambiguous for BiHashMap; use `aValues` or `bValues`, instead")
  }

  @deprecated("Using this will throw an exception!  Use `aValues` or `bValues` instead.", "forever")
  override def values : collection.Iterable[B] = {
    throw new UnsupportedOperationException("`values` function ambiguous for BiHashMap; use `aValues` or `bValues`, instead")
  }

}

object BiHashMap extends MutableMapFactory[BiHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), BiHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: BiHashMap[A, B] = new BiHashMap[A, B]()
}
