package datastructure

import scala.deprecated
import utilitylib.typewarfare.TypeWarfare.||
import collection.generic.CanBuildFrom
import collection.mutable.{Builder, Map}

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
  def filterAs(p: (A) => Boolean) : this.type
  def mapAs[C](f: (A) => C)       : Map[C, B]  //@ I'd like to return essentially `this.type[C, B]`, but that'll require some `Repr` magic, I think

  def aIterator: Iterator[A]
  def aSet:      collection.immutable.Set[A]
  def aValues:   Iterable[A]


  // Abstracts for Bs
  def filterBs(p: (B) => Boolean) : this.type
  def mapBs[C](f: (B) => C)       : Map[A, C]  //@ I'd like to return essentially `this.type[A, C]`, but that'll require some `Repr` magic, I think

  def bIterator: Iterator[B]
  def bSet:      collection.immutable.Set[B]
  def bValues:   Iterable[B]


  def sameElements[U : ((A, B) || (B, A))#T, C >: U](that: collection.GenIterable[C]) : Boolean = (abMap sameElements that) || (baMap sameElements that)


  //@ An algebraic turd...
  override def mapResult[NewTo](f: Repr => NewTo) : Builder[(A, B), NewTo] = {
    new Builder[(A, B), NewTo] with Proxy {
      val self = Builder.this
      def +=(x: (A, B)): this.type = { self += x; this }
      def clear() = self.clear()
      override def ++=(xs: TraversableOnce[(A, B)]): this.type = { self ++= xs; this }
      def result(): NewTo = f(self.result())
    }
  }


  // Purposely-broken methods
  @deprecated(CommonDeprecationStrFormat("filterAs", "filterBs"), CommonDeprecationSinceVersion)
  override def filterKeys(p: (A) => Boolean) : Map[A, B] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("filterKeys", "filterAs", "filterBs"))

  @deprecated(CommonDeprecationStrFormat("mapAs", "mapBs"), CommonDeprecationSinceVersion)
  override def mapValues[C](f: (B) => C) : Map[A, C] = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("mapValues", "mapAs", "mapBs"))

  @deprecated(CommonDeprecationStrFormat("minBy"), CommonDeprecationSinceVersion)
  override def min[B >: A](implicit cmp: Ordering[B]) : A = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("min", "minBy"))

  @deprecated(CommonDeprecationStrFormat("maxBy"), CommonDeprecationSinceVersion)
  override def max[B >: A](implicit cmp: Ordering[B]) : A = throw new UnsupportedOperationException(CommonUnsupportedOpExMsgFormat("max", "maxBy"))

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
