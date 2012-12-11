package datastructure.mutable

import scala.deprecated
import collection.mutable.{ Map => MMap, MapLike }

import utilitylib.typewarfare.TypeWarfare.||

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/12/12
 * Time: 11:33 PM
 */

trait Bijection[A, B, M[X, Y] <: MMap[X, Y], Rpr[X, Y] <: Bijection[X, Y, M, Rpr]]
  extends MMap[A, B]
    with MapLike[A, B, Rpr[A, B]]
    with BijectionForwardOps[A, B, M, Rpr]
    with BijectionReverseOps[A, B, M, Rpr]
    with Equals
{

  protected type Repr = Rpr[A, B]
  private   type Tup  = (A, B)

  protected def abMap:   M[A, B]
  protected def baMap:   M[B, A]
  protected def myEmpty: Repr  //@ Yuck!  Let's find a way around this one day!  Done because, in this trait, I don't see how we can provide a base implementation of `empty`.
                               // If we don't give one, though, `MapLike` and `MMap` get us an implementation that is incompatible with `Rpr`.

  override def clear()             { abMap.clear(); baMap.clear() }
  override def empty      : Repr =   myEmpty
  override def hashCode() : Int  =   abMap.hashCode() ^ baMap.hashCode()   // XOR the hashcodes of the two maps
  override def size       : Int  = { require (abMap.size == baMap.size); abMap.size }

  override def canEqual(other: Any) : Boolean
  override def equals(that: Any)    : Boolean  = {
    that match {
      case b: Bijection[_, _, _, _] => (b canEqual this) && ((b.abMap.equals(abMap) && b.baMap.equals(baMap)) || (b.abMap.equals(baMap) && b.baMap.equals(abMap)))
      case _                        => false
    }
  }

  def iterator : Iterator[Tup] = abMap.iterator

  // Lambda-operation methods
  override def /:[C]                      (z: C)(op: (C, Tup) => C)                          : C           =   abMap./:(z)(op)
  override def /:\[A1 >: Tup]             (z: A1)(op: (A1, A1) => A1)                        : A1          =   abMap./:\(z)(op)
  override def :\[C]                      (z: C)(op: (Tup, C) => C)                          : C           =   abMap.:\(z)(op)
  override def aggregate[C]               (z: C)(seqop: (C, Tup) => C, combop: (C, C) => C)  : C           =   abMap.aggregate(z)(seqop, combop)
  override def collectFirst[C]            (pf: PartialFunction[Tup, C])                      : Option[C]   =   abMap collectFirst pf
  override def count                      (p: (Tup) => Boolean)                              : Int         =   abMap count p
  override def exists                     (p: (Tup) => Boolean)                              : Boolean     =   abMap exists p
  override def find                       (p: (Tup) => Boolean)                              : Option[Tup] =   abMap find p
  override def fold[A1 >: Tup]            (z: A1)(op: (A1, A1) => A1)                        : A1          =   abMap.fold(z)(op)
  override def foldLeft[C]                (z: C)(op: (C, Tup) => C)                          : C           =   abMap.foldLeft(z)(op)
  override def foldRight[C]               (z: C)(op: (Tup, C) => C)                          : C           =   abMap.foldRight(z)(op)
  override def forall                     (p: (Tup) => Boolean)                              : Boolean     =   abMap forall p
  override def foreach[C]                 (f: (Tup) => C)                                                    { abMap.foreach(f) }
  override def minBy[C]                   (f: (Tup) => C)(implicit cmp: Ordering[C])         : Tup         =   abMap.minBy(f)
  override def maxBy[C]                   (f: (Tup) => C)(implicit cmp: Ordering[C])         : Tup         =   abMap.maxBy(f)
  override def reduce[C >: Tup]           (op: (C, C) => C)                                  : C           =   abMap reduce op
  override def reduceLeft[C >: Tup]       (op: (C, Tup) => C)                                : C           =   abMap reduceLeft op
  override def reduceLeftOption[C >: Tup] (op: (C, Tup) => C)                                : Option[C]   =   abMap reduceLeftOption op
  override def reduceOption[C >: Tup]     (op: (C, C) => C)                                  : Option[C]   =   abMap reduceOption op
  override def reduceRight[C >: Tup]      (op: (Tup, C) => C)                                : C           =   abMap reduceRight op
  override def reduceRightOption[C >: Tup](op: (Tup, C) => C)                                : Option[C]   =   abMap reduceRightOption op
  override def retain                     (p: (A, B) => Boolean)                             : this.type   = { this.seq foreach { case (k, v) => if (!p(k, v)) this -= k }; this }
  override def transform                  (f: (A, B) => B)                                   : this.type   = { this.iterator foreach { case (k, v) => update(k, f(k, v)) }; this }
  override def withDefault                (d: A => B)                                        : MMap[A, B]  =   abMap withDefault d   //@ I'd love to do this with a better return type...

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
