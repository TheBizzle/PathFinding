package datastructure.mutable

import collection.{ generic, GenTraversableOnce }, generic.CanBuildFrom

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:41 PM
 */

// It would be great if there were something that made this task easier--something like `Proxy`s.
// But I'm not aware of any common pattern that allows me to have two different traits inheriting
// the same functionality and wrapping it in different ways, with different type signatures (to avoid type erasure).
// Utilizes `DummyImplicit` (from Predef) to give the methods herein different signatures to the JVM than the `_ForwardsOps` ones get
trait BiHashReverseOps[A, B] {

  self: BiHashMap[A, B] =>

  private type BTup = (B, A)

  private val implWrapper = new BiHashImplWrapper(baMap, abMap, repr)

  // General manipulation operators
  //@ I'm uneasy about this usage of `BiHashMap` in the return type
  def + [A1 >: A](ba: (B, A1))                              (implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] += ba
  def + [A1 >: A](ba1: (B, A1), ba2: (B, A1), bas: (B, A1)*)(implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] += ba1 += ba2 ++= bas
  def ++[A1 >: A](bas: GenTraversableOnce[(B, A1)])         (implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] ++= bas.seq
  def -          (bKey: B)                                  (implicit ignore: DummyImplicit) : Repr             = clone() -= bKey
  def -          (bKey1: B, bKey2: B, bKeys: B*)            (implicit ignore: DummyImplicit) : Repr             = clone() -= bKey1 -= bKey2 --= bKeys
  def --         (bKeys: GenTraversableOnce[B])             (implicit ignore: DummyImplicit) : Repr             = clone() --= bKeys.seq

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
  def apply             (bKey: B)                (implicit ignore: DummyImplicit) : A                = implWrapper.apply(bKey)
  def contains          (bKey: B)                (implicit ignore: DummyImplicit) : Boolean          = implWrapper.contains(bKey)
  def default           (bKey: B)                (implicit ignore: DummyImplicit) : A                = implWrapper.default(bKey)
  def get               (bKey: B)                (implicit ignore: DummyImplicit) : Option[A]        = implWrapper.get(bKey)
  def getOrElse[A1 >: A](bKey: B, default: => A1)(implicit ignore: DummyImplicit) : A1               = implWrapper.getOrElse(bKey, default)
  def getOrElseUpdate   (bKey: B, aOp: => A)     (implicit ignore: DummyImplicit) : A                = implWrapper.getOrElseUpdate(bKey, aOp)
  def isDefinedAt       (bKey: B)                (implicit ignore: DummyImplicit) : Boolean          = implWrapper.isDefinedAt(bKey)
  def put               (bKey: B, aVal: A)       (implicit ignore: DummyImplicit) : Option[A]        = implWrapper.put(bKey, aVal)
  def remove            (bKey: B)                (implicit ignore: DummyImplicit) : Option[A]        = implWrapper.remove(bKey)
  def update            (bKey: B, aVal: A)       (implicit ignore: DummyImplicit)                    { implWrapper.update(bKey, aVal) }
  def updated[A1 >: A]  (bKey: B, aVal: A1)      (implicit ignore: DummyImplicit) : BiHashMap[A1, B] =   this + ((bKey, aVal))

  // Function-chaining methods
  def andThen[C]              (k: (A) => C)(implicit ignore: DummyImplicit)                   : PartialFunction[B, C]   = implWrapper andThen k
  def compose[C]              (g: (C) => B)(implicit ignore: DummyImplicit)                   : (C) => A                = implWrapper compose g
  def orElse[B1 <: B, A1 >: A](that: PartialFunction[B1, A1])(implicit ignore: DummyImplicit) : PartialFunction[B1, A1] = implWrapper orElse that

  //@ Fix
  override def filterBs(p: (B) => Boolean) : collection.Map[A, B] = implWrapper filterKeys p map (_.swap)
  override def mapAs[C](f: (A) => C)       : collection.Map[C, B] = implWrapper mapValues  f map (_.swap)

  // Collection-morphing methods
  //@ Should `Bijection` have base implementations of these things?  Seems probable
  override def bIterator : Iterator[B]       = implWrapper.keysIterator
  override def bSet      : collection.Set[B] = implWrapper.keySet
  override def bValues   : Iterable[B]       = implWrapper.keys

  def copyToArray[C >: BTup] (xs: Array[C])                      (implicit ignore: DummyImplicit) { implWrapper.copyToArray(xs) }
  def copyToArray[C >: BTup] (xs: Array[C], start: Int)          (implicit ignore: DummyImplicit) { implWrapper.copyToArray(xs, start) }
  def copyToArray[C >: BTup] (xs: Array[C], start: Int, len: Int)(implicit ignore: DummyImplicit) { implWrapper.copyToArray(xs, start, len) }
  def copyToBuffer[C >: BTup](dest: collection.mutable.Buffer[C])(implicit ignore: DummyImplicit) { implWrapper.copyToBuffer(dest) }

}
