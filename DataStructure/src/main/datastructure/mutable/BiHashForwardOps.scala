package datastructure.mutable

import collection.{ generic, GenTraversableOnce }, generic.CanBuildFrom

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] {

  self: BiHashMap[A, B] =>

  private type FTup = (A, B)

  private val implWrapper = new BiHashImplWrapper(abMap, baMap, repr)

  // General manipulation operators
  //@ I'm uneasy about this usage of `BiHashMap` in the return type
  override def + [B1 >: B](ab: (A, B1))                               : BiHashMap[A, B1] = clone().asInstanceOf[BiHashMap[A, B1]] += ab
  override def + [B1 >: B](ab1: (A, B1), ab2: (A, B1), abs: (A, B1)*) : BiHashMap[A, B1] = clone().asInstanceOf[BiHashMap[A, B1]] += ab1 += ab2 ++= abs
  override def ++[B1 >: B](abs: GenTraversableOnce[(A, B1)])          : BiHashMap[A, B1] = clone().asInstanceOf[BiHashMap[A, B1]] ++= abs.seq
  override def -          (aKey: A)                                   : Repr             = clone() -= aKey
  override def -          (aKey1: A, aKey2: A, aKeys: A*)             : Repr             = clone() -= aKey1 -= aKey2 --= aKeys
  override def --         (aKeys: GenTraversableOnce[A])              : Repr             = clone() --= aKeys.seq

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
  override def apply             (aKey: A)                 : B                = implWrapper.apply(aKey)
  override def contains          (aKey: A)                 : Boolean          = implWrapper.contains(aKey)
  override def default           (aKey: A)                 : B                = implWrapper.default(aKey)
  override def get               (aKey: A)                 : Option[B]        = implWrapper.get(aKey)
  override def getOrElse[B1 >: B](aKey: A, default: => B1) : B1               = implWrapper.getOrElse(aKey, default)
  override def getOrElseUpdate   (aKey: A, bOp: => B)      : B                = implWrapper.getOrElseUpdate(aKey, bOp)
  override def isDefinedAt       (aKey: A)                 : Boolean          = implWrapper.isDefinedAt(aKey)
  override def put               (aKey: A, bVal: B)        : Option[B]        = implWrapper.put(aKey, bVal)
  override def remove            (aKey: A)                 : Option[B]        = implWrapper.remove(aKey)
  override def update            (aKey: A, bVal: B)                           { implWrapper.update(aKey, bVal) }
  override def updated[B1 >: B]  (aKey: A, bVal: B1)       : BiHashMap[A, B1] = this + ((aKey, bVal))

  // Function-chaining methods
  override def andThen[C]              (k: (B) => C)                   : PartialFunction[A, C]   = implWrapper andThen k
  override def compose[C]              (g: (C) => A)                   : (C) => B                = implWrapper compose g
  override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] = implWrapper orElse that

  //@ Fix
  override def filterAs(p: (A) => Boolean) : collection.Map[A, B] = implWrapper filterKeys p
  override def mapBs[C](f: (B) => C)       : collection.Map[A, C] = implWrapper mapValues  f

  // Collection-morphing methods
  override def aIterator : Iterator[A]       = implWrapper.keysIterator
  override def aSet      : collection.Set[A] = implWrapper.keySet
  override def aValues   : Iterable[A]       = implWrapper.keys

  // Copying methods
  override def copyToArray[C >: FTup] (xs: Array[C])                       { implWrapper.copyToArray(xs) }
  override def copyToArray[C >: FTup] (xs: Array[C], start: Int)           { implWrapper.copyToArray(xs, start) }
  override def copyToArray[C >: FTup] (xs: Array[C], start: Int, len: Int) { implWrapper.copyToArray(xs, start, len) }
  override def copyToBuffer[C >: FTup](dest: collection.mutable.Buffer[C]) { implWrapper.copyToBuffer(dest) }

}
