package datastructure

import scala.deprecated
import utilitylib.typewarfare.TypeWarfare.||
import collection.mutable.Map

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/12/12
 * Time: 11:33 PM
 */

abstract class Bijection[A, B, M[X, Y] <: Map[X, Y], MAB <: M[A, B], MBA <: M[B, A], Rpr <: Map[A, B]](protected val abMap: MAB, protected val baMap: MBA) extends Map[A, B] with Equals {

  protected type Repr = Rpr

  override def hashCode() : Int                =   abMap.hashCode() ^ baMap.hashCode()   // XOR the hashcodes of the two maps
  override def clear()                           { abMap.clear(); baMap.clear() }
  override def size       : Int                = { require (abMap.size == baMap.size); abMap.size }

  override def canEqual(other: Any) : Boolean
  override def equals(that: Any)    : Boolean  = {
    that match {
      case b: Bijection[_, _, _, _, _, _] => (b canEqual this) &&
                                             ( (b.abMap.equals(abMap) && b.baMap.equals(baMap)) ||
                                               (b.abMap.equals(baMap) && b.baMap.equals(abMap)) )
      case _                              => false
    }
  }

  def iterator : Iterator[(A, B)] = abMap.iterator


  // Abstracts for As
  def filterAs(p: (A) => Boolean) : collection.Map[A, B]
  def mapAs[C](f: (A) => C)       : collection.Map[C, B]  //@ I'd like to return essentially `this.type[C, B]`, but that'll require some `Repr` magic, I think

  def aIterator: Iterator[A]
  def aSet:      collection.Set[A]
  def aValues:   Iterable[A]


  // Abstracts for Bs
  def filterBs(p: (B) => Boolean) : collection.Map[A, B]
  def mapBs[C](f: (B) => C)       : collection.Map[A, C]  //@ I'd like to return essentially `this.type[A, C]`, but that'll require some `Repr` magic, I think

  def bIterator: Iterator[B]
  def bSet:      collection.Set[B]
  def bValues:   Iterable[B]

  def sameElements[U : ((A, B) || (B, A))#T, C >: U](that: collection.GenIterable[C]) : Boolean = (abMap sameElements that) || (baMap sameElements that)

  // Purposely-broken methods
  @deprecated(CommonDeprecationStrFormat("filterAs", "filterBs"), CommonDeprecationSinceVersion)
  override def filterKeys(p: (A) => Boolean) : Map[A, B] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("filterKeys", "filterAs", "filterBs"))

  @deprecated(CommonDeprecationStrFormat("mapAs", "mapBs"), CommonDeprecationSinceVersion)
  override def mapValues[C](f: (B) => C) : Map[A, C] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("mapValues", "mapAs", "mapBs"))

  @deprecated(CommonDeprecationStrFormat("minBy"), CommonDeprecationSinceVersion)
  override def min[C >: (A, B)](implicit cmp: Ordering[C]) : (A, B) = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("min", "minBy"))

  @deprecated(CommonDeprecationStrFormat("maxBy"), CommonDeprecationSinceVersion)
  override def max[C >: (A, B)](implicit cmp: Ordering[C]) : (A, B) = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("max", "maxBy"))

  @deprecated(CommonDeprecationStrFormat("aSet", "bSet"), CommonDeprecationSinceVersion)
  override def keySet : collection.immutable.Set[A] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("keySet", "aSet", "bSet"))

  @deprecated(CommonDeprecationStrFormat("aValues", "bValues"), CommonDeprecationSinceVersion)
  override def keys : collection.Iterable[A] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("keys", "aValues", "bValues"))

  @deprecated(CommonDeprecationStrFormat("aValues", "bValues"), CommonDeprecationSinceVersion)
  override def values : collection.Iterable[B] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("values", "aValues", "bValues"))

  @deprecated(CommonDeprecationStrFormat("aIterator", "bIterator"), CommonDeprecationSinceVersion)
  override def valuesIterator: Iterator[B] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("valuesIterator", "aIterator", "bIterator"))

  // Silly constants
  private def CommonUnsupportedOpExMsgFormat(x: String, xs: String*) : String = ("`" + x + "` function ambiguous for " + this.getClass.getName + "; use %s instead").format(xs.mkString("`", "` or `", "`"))
  private def CommonDeprecationStrFormat(xs: String*) : String = "Using this will throw an exception!  Use %s instead.".format(xs.mkString("`", "` or `", "`"))
  private val CommonDeprecationSinceVersion = "forever"

}
