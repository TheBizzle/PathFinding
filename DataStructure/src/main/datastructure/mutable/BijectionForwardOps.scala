package datastructure.mutable

import
  scala.collection.{ generic, GenTraversableOnce, mutable },
    generic.CanBuildFrom,
    mutable.{ Map => MMap }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/12
 * Time: 3:56 PM
 */

private[mutable] trait BijectionForwardOps[A, B, M[X, Y] <: MMap[X, Y], R[X, Y] <: Bijection[X, Y, M, R]] {

  self: Bijection[A, B, M, R] =>

  private type FTup = (A, B)

  private val fImplWrapper = new BijectionImplWrapper(abMap, baMap)

  // General manipulation operators
  override def + [B1 >: B](ab: (A, B1))                               : R[A, B1] = clone().asInstanceOf[R[A, B1]] += ab
  override def + [B1 >: B](ab1: (A, B1), ab2: (A, B1), abs: (A, B1)*) : R[A, B1] = clone().asInstanceOf[R[A, B1]] += ab1 += ab2 ++= abs
  override def ++[B1 >: B](abs: GenTraversableOnce[(A, B1)])          : R[A, B1] = clone().asInstanceOf[R[A, B1]] ++= abs.seq
  override def -          (aKey: A)                                   : Repr     = clone() -= aKey
  override def -          (aKey1: A, aKey2: A, aKeys: A*)             : Repr     = clone() -= aKey1 -= aKey2 --= aKeys
  override def --         (aKeys: GenTraversableOnce[A])              : Repr     = clone() --= aKeys.seq

  // Satanry
  override def ++:[C >: FTup, That](that: TraversableOnce[C])(implicit bf: CanBuildFrom[Repr, C, That]) : That = {
    val b = bf(repr)
    if (that.isInstanceOf[collection.IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection
    b.result
  }

  // Self-return operators
  override def += (ab: FTup)                         : this.type = { put(ab._1, ab._2); this }
  override def += (ab1: FTup, ab2: FTup, abs: FTup*) : this.type =   this += ab1 += ab2 ++= abs
  override def ++=(abs: TraversableOnce[FTup])       : this.type = { abs.seq foreach += ; this }
  override def -= (aKey: A)                          : this.type = { remove(aKey); this }
  override def -= (aKey1: A, aKey2: A, aKeys: A*)    : this.type = { this -= aKey1; this -= aKey2; this --= aKeys }
  override def --=(abs: TraversableOnce[A])          : this.type = { abs.seq foreach -= ; this }

  // General methods
  override def apply             (aKey: A)                 : B         = fImplWrapper.apply(aKey)
  override def contains          (aKey: A)                 : Boolean   = fImplWrapper.contains(aKey)
  override def default           (aKey: A)                 : B         = fImplWrapper.default(aKey)
  override def get               (aKey: A)                 : Option[B] = fImplWrapper.get(aKey)
  override def getOrElse[B1 >: B](aKey: A, default: => B1) : B1        = fImplWrapper.getOrElse(aKey, default)
  override def getOrElseUpdate   (aKey: A, bOp: => B)      : B         = fImplWrapper.getOrElseUpdate(aKey, bOp)
  override def isDefinedAt       (aKey: A)                 : Boolean   = fImplWrapper.isDefinedAt(aKey)
  override def put               (aKey: A, bVal: B)        : Option[B] = fImplWrapper.put(aKey, bVal)
  override def remove            (aKey: A)                 : Option[B] = fImplWrapper.remove(aKey)
  override def update            (aKey: A, bVal: B)                    { fImplWrapper.update(aKey, bVal) }
  override def updated[B1 >: B]  (aKey: A, bVal: B1)       : R[A, B1]  = this + ((aKey, bVal))

  // Function-chaining methods
  override def andThen[C]              (k: (B) => C)                   : PartialFunction[A, C]   = fImplWrapper andThen k
  override def compose[C]              (g: (C) => A)                   : (C) => B                = fImplWrapper compose g
  override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] = fImplWrapper orElse that

  //@ Fix
  def filterAs(p: (A) => Boolean) : collection.Map[A, B] = fImplWrapper filterKeys p
  def mapBs[C](f: (B) => C)       : collection.Map[A, C] = fImplWrapper mapValues  f

  // Collection-morphing methods
  def aIterator : Iterator[A]       = fImplWrapper.keysIterator
  def aSet      : collection.Set[A] = fImplWrapper.keySet
  def aValues   : Iterable[A]       = fImplWrapper.keys

  // Copying methods
  override def copyToArray[C >: FTup] (xs: Array[C])                       { fImplWrapper.copyToArray(xs) }
  override def copyToArray[C >: FTup] (xs: Array[C], start: Int)           { fImplWrapper.copyToArray(xs, start) }
  override def copyToArray[C >: FTup] (xs: Array[C], start: Int, len: Int) { fImplWrapper.copyToArray(xs, start, len) }
  override def copyToBuffer[C >: FTup](dest: collection.mutable.Buffer[C]) { fImplWrapper.copyToBuffer(dest) }

}
