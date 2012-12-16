package datastructure.mutable

import collection.{ generic, GenTraversableOnce }, generic.CanBuildFrom
import collection.mutable.{ Map => MMap }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/12
 * Time: 1:39 PM
 */

private[mutable] trait BijectionReverseOps[A, B, M[X, Y] <: MMap[X, Y], R[X, Y] <: Bijection[X, Y, M, R]] {

  self: Bijection[A, B, M, R] =>

  protected type BTup = (B, A)

  protected val bImplWrapper = new BijectionImplWrapper(baMap, abMap)

  // General manipulation operators
  def + [A1 >: A](ba: (B, A1))                              (implicit ignore: DummyImplicit) : R[A1, B]
  def + [A1 >: A](ba1: (B, A1), ba2: (B, A1), bas: (B, A1)*)(implicit ignore: DummyImplicit) : R[A1, B]
  def ++[A1 >: A](bas: GenTraversableOnce[(B, A1)])         (implicit ignore: DummyImplicit) : R[A1, B]
  def -          (bKey: B)                                  (implicit ignore: DummyImplicit) : Repr
  def -          (bKey1: B, bKey2: B, bKeys: B*)            (implicit ignore: DummyImplicit) : Repr
  def --         (bKeys: GenTraversableOnce[B])             (implicit ignore: DummyImplicit) : Repr

  // Satanry
  def ++:[C >: BTup, That](that: TraversableOnce[C])(implicit bf: CanBuildFrom[Repr, C, That], ignore: DummyImplicit) : That = {
    val b = bf(repr)
    if (that.isInstanceOf[collection.IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection map (_.swap)
    b.result
  }

  // Self-return operators
  def += (ba: BTup)                        (implicit ignore: DummyImplicit) : this.type = { put(ba._1, ba._2); this }
  def += (ba1: BTup, ba2: BTup, bas: BTup*)(implicit ignore: DummyImplicit) : this.type =   this += ba1 += ba2 ++= bas
  def ++=(bas: TraversableOnce[BTup])      (implicit ignore: DummyImplicit) : this.type = { bas.seq foreach += ; this }
  def -= (bKey: B)                         (implicit ignore: DummyImplicit) : this.type = { remove(bKey); this }
  def -= (bKey1: B, bKey2: B, bKeys: B*)   (implicit ignore: DummyImplicit) : this.type = { this -= bKey1; this -= bKey2; this --= bKeys }
  def --=(bas: TraversableOnce[B])         (implicit ignore: DummyImplicit) : this.type = { bas.seq foreach -= ; this }

  // General methods
  def apply             (bKey: B)                (implicit ignore: DummyImplicit) : A         = bImplWrapper.apply(bKey)
  def contains          (bKey: B)                (implicit ignore: DummyImplicit) : Boolean   = bImplWrapper.contains(bKey)
  def default           (bKey: B)                (implicit ignore: DummyImplicit) : A         = bImplWrapper.default(bKey)
  def get               (bKey: B)                (implicit ignore: DummyImplicit) : Option[A] = bImplWrapper.get(bKey)
  def getOrElse[A1 >: A](bKey: B, default: => A1)(implicit ignore: DummyImplicit) : A1        = bImplWrapper.getOrElse(bKey, default)
  def getOrElseUpdate   (bKey: B, aOp: => A)     (implicit ignore: DummyImplicit) : A         = bImplWrapper.getOrElseUpdate(bKey, aOp)
  def isDefinedAt       (bKey: B)                (implicit ignore: DummyImplicit) : Boolean   = bImplWrapper.isDefinedAt(bKey)
  def put               (bKey: B, aVal: A)       (implicit ignore: DummyImplicit) : Option[A] = bImplWrapper.put(bKey, aVal)
  def remove            (bKey: B)                (implicit ignore: DummyImplicit) : Option[A] = bImplWrapper.remove(bKey)
  def update            (bKey: B, aVal: A)       (implicit ignore: DummyImplicit)             { bImplWrapper.update(bKey, aVal) }
  def updated[A1 >: A]  (bKey: B, aVal: A1)      (implicit ignore: DummyImplicit) : R[A1, B]  = this + ((bKey, aVal))

  // Function-chaining methods
  def andThen[C]              (k: (A) => C)(implicit ignore: DummyImplicit)                   : PartialFunction[B, C]   = bImplWrapper andThen k
  def compose[C]              (g: (C) => B)(implicit ignore: DummyImplicit)                   : (C) => A                = bImplWrapper compose g
  def orElse[B1 <: B, A1 >: A](that: PartialFunction[B1, A1])(implicit ignore: DummyImplicit) : PartialFunction[B1, A1] = bImplWrapper orElse that

  //@ Fix
  def filterBs(p: (B) => Boolean) : collection.Map[A, B] = bImplWrapper filterKeys p map (_.swap)
  def mapAs[C](f: (A) => C)       : collection.Map[C, B] = bImplWrapper mapValues  f map (_.swap)

  // Collection-morphing methods
  def bIterator : Iterator[B]       = bImplWrapper.keysIterator
  def bSet      : collection.Set[B] = bImplWrapper.keySet
  def bValues   : Iterable[B]       = bImplWrapper.keys

  def copyToArray[C >: BTup] (xs: Array[C])                      (implicit ignore: DummyImplicit) { bImplWrapper.copyToArray(xs) }
  def copyToArray[C >: BTup] (xs: Array[C], start: Int)          (implicit ignore: DummyImplicit) { bImplWrapper.copyToArray(xs, start) }
  def copyToArray[C >: BTup] (xs: Array[C], start: Int, len: Int)(implicit ignore: DummyImplicit) { bImplWrapper.copyToArray(xs, start, len) }
  def copyToBuffer[C >: BTup](dest: collection.mutable.Buffer[C])(implicit ignore: DummyImplicit) { bImplWrapper.copyToBuffer(dest) }

}
