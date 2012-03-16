package datastructure.mutable

import collection.generic.{CanBuildFrom, MutableMapFactory}
import datastructure.Bijection
import java.lang.IllegalStateException
import collection.mutable.MapLike


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/12/12
 * Time: 9:35 PM
 */

class BiHashMap[A, B] private[datastructure](contents: (A, B)*)
	  extends Bijection[A, B](contents: _*)
   	with Map[A, B]
    with MapLike[A, B, BiHashMap[A, B]]
//@	   with CustomParallelizable[(A, B), ParHashMap[A, B]]
	{
  
	  override def empty: BiHashMap[A, B] = BiHashMap.empty[A, B]
	  override def clear() { abMap.clear(); baMap.clear() }
	  override def size: Int = { if (abMap.size == baMap.size) abMap.size else throw new IllegalStateException("Submap size mismatch!  Corrupted BiHashMap detected!") }

	  def this() = this(null)

//@	  override def par = new ParHashMap[A, B](hashTableContents)

  def get(key: A) : Option[B] = {
    abMap.get(key)
  }

  def get(key: => B) : Option[A] = {
    val b = key
    baMap.get(b)
  }

  override def put(key: A, value: B) : Option[B] = {
	  baMap.put(value, key)
    abMap.put(key, value)
  }

  def put(key: => B, value: A) : Option[A] = {
    val k = key
    abMap.put(value, k)
    baMap.put(k, value)
  }

  override def update(key: A, value: B) {
    val hold = abMap.get(key)
    abMap.update(key, value)
    hold foreach { b => baMap.remove(b); baMap.put(value, key) }
  }

  def update(key: => B, value: A) {
    val b = key
    val hold = baMap.get(b)
    baMap.update(b, value)
    hold foreach { a => abMap.remove(a); abMap.put(value, b) }
  }

  override def remove(key: A) : Option[B] = {
    abMap.get(key) foreach (baMap.remove(_))
    abMap.remove(key)
  }

  def remove(key: B) : Option[A] = {
    val k = key
    baMap.get(k) foreach (abMap.remove(_))
    baMap.remove(k)
  }

  def += (ab: (A,  B)) : this.type = {
    put(ab._1, ab._2)
    this
  }

  def += (ba: => (B,  A)) : this.type = {
    val baTuple = ba; import baTuple._
    put(_1, _2)
    this
  }

  def -=(key: A) : this.type = {
    remove(key)
    this
  }

  def -=(key: => B) : this.type = {
    remove(key)
    this
  }

  def iterator : Iterator[(A, B)] = {
    abMap.iterator
  }

  override def foreach[C](f: ((A, B)) => C) {
    abMap foreach f
  }

  @deprecated("Using this will throw an exception!  Use `aValues` or `bValues` instead.", "forever")
  override def keySet : collection.Set[A] = new DefaultKeySet {
    throw new UnsupportedOperationException("`keySet` function ambiguous for BiHashMap; use `aValues` or `bValues`, instead")
	}

  @deprecated("Using this will throw an exception!  Use `aValues` or `bValues` instead.", "forever")
	override def values : collection.Iterable[B] = new DefaultValuesIterable {
	  throw new UnsupportedOperationException("`values` function ambiguous for BiHashMap; use `aValues` or `bValues`, instead")
  }

  def aValues : scala.collection.Set[A] = {
    abMap.keySet
  }

  def bValues : scala.collection.Set[B] = {
    baMap.keySet
  }

  def aIterator : Iterator[A] = {
    abMap.keysIterator
  }

  def bIterator : Iterator[B] = {
    baMap.keysIterator
  }

  /** Toggles whether a size map is used to track hash map statistics for the child maps.
   */
  def useSizeMap(t: Boolean) {
    abMap.useSizeMap(t)
    baMap.useSizeMap(t)
  }

}

object BiHashMap extends MutableMapFactory[BiHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), BiHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: BiHashMap[A, B] = new BiHashMap[A, B]
}
