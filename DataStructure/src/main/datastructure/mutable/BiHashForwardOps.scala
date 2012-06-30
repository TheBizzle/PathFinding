package datastructure.mutable

import collection.generic.CanBuildFrom
import collection.GenTraversableOnce

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] {

  self: BiHashMap[A, B] =>

  private type Tup = (A, B)

  private val implWrapper = new BiHashImplWrapper(abMap, baMap)

  // General manipulation operators
  //@ I'm uneasy about this usage of `BiHashMap` in the return type
  override def + [B1 >: B](ab: (A, B1))                               : BiHashMap[A, B1] = clone().asInstanceOf[BiHashMap[A, B1]] += ab
  override def + [B1 >: B](ab1: (A, B1), ab2: (A, B1), abs: (A, B1)*) : BiHashMap[A, B1] = clone().asInstanceOf[BiHashMap[A, B1]] += ab1 += ab2 ++= abs
  override def ++[B1 >: B](abs: GenTraversableOnce[(A, B1)])          : BiHashMap[A, B1] = clone().asInstanceOf[BiHashMap[A, B1]] ++= abs.seq
  override def -          (aKey: A)                                   : this.type        = clone() -= aKey
  override def -          (aKey1: A, aKey2: A, aKeys: A*)             : this.type        = clone() -= aKey1 -= aKey2 --= aKeys
  override def --         (aKeys: GenTraversableOnce[A])              : this.type        = clone() --= aKeys.seq

  // Satanry
  //@ There is no `That`!
  override def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]) : That = {
    val b = bf(repr)
    if (that.isInstanceOf[collection.IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection
    b.result
  }

  // Self-return operators
  override def += (ab: Tup)                       : this.type = { put(ab._1, ab._2); this }
  override def += (ab1: Tup, ab2: Tup, abs: Tup*) : this.type =   this += ab1 += ab2 ++= abs
  override def ++=(abs: TraversableOnce[Tup])     : this.type = { abs.seq foreach += ; this }
  override def -= (aKey: A)                       : this.type = { remove(aKey); this }
  override def -= (aKey1: A, aKey2: A, aKeys: A*) : this.type = { this -= aKey1; this -= aKey2; this --= aKeys }
  override def --=(abs: TraversableOnce[Tup])     : this.type = { abs.seq foreach -= ; this }

  // General methods
  override def apply             (aKey: A)                 : B         =   implWrapper.apply(aKey)
  override def contains          (aKey: A)                 : Boolean   =   implWrapper.contains(aKey)
  override def default           (aKey: A)                 : B         =   implWrapper.default(aKey)
  override def get               (aKey: A)                 : Option[B] =   implWrapper.get(aKey)
  override def getOrElse[B1 >: B](aKey: A, default: => B1) : B1        =   implWrapper.getOrElse(aKey, default)
  override def getOrElseUpdate   (aKey: A, bOp: => B)      : B         =   implWrapper.getOrElseUpdate(aKey, bOp)
  override def isDefinedAt       (aKey: A)                 : Boolean   =   implWrapper.isDefinedAt(aKey)
  override def put               (aKey: A, bVal: B)        : Option[B] =   implWrapper.put(aKey, bVal)
  override def remove            (aKey: A)                 : Option[B] =   implWrapper.remove(aKey)
  override def update            (aKey: A, bVal: B)                      { implWrapper.update(aKey, bVal) }
  override def updated[B1 >: B]  (aKey: A, bVal: B1)       : this.type =   this + (aKey, bVal)

  // Function-chaining methods
  override def andThen[C]              (k: (B) => C)                   : PartialFunction[A, C]   = implWrapper andThen k
  override def compose[C]              (g: (C) => A)                   : (C) => B                = implWrapper compose g
  override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] = implWrapper orElse that

  //@ Am I doing the first type parameter for these `CanBuildFrom`s correctly...?
  // override def collect[C, That](pf: PartialFunction[(A, B), C])(implicit bf: CanBuildFrom[this.type, C, That]) : That = implWrapper.collect(pf)

  // Lambda-operation methods
  override def /:[C]                      (z: C)(op: (C, Tup) => C)                          : C           =   implWrapper./:(z)(op)
  override def /:\[A1 >: Tup]             (z: A1)(op: (A1, A1) => A1)                        : A1          =   implWrapper./:\(z)(op)
  override def :\[C]                      (z: C)(op: (Tup, C) => C)                          : C           =   implWrapper.:\(z)(op)
  override def aggregate[C]               (z: C)(seqop: (C, Tup) => C, combop: (C, C) => C)  : C           =   implWrapper.aggregate(z)(seqop, combop)
  override def count                      (p: (Tup) => Boolean)                              : Int         =   implWrapper count p
  override def exists                     (p: (Tup) => Boolean)                              : Boolean     =   implWrapper exists p
  override def find                       (p: (Tup) => Boolean)                              : Option[Tup] =   implWrapper find p
  override def fold[A1 >: Tup]            (z: A1)(op: (A1, A1) => A1)                        : A1          =   implWrapper.fold(z)(op)
  override def foldLeft[C]                (z: C)(op: (C, Tup) => C)                          : C           =   implWrapper.foldLeft(z)(op)
  override def foldRight[C]               (z: C)(op: (Tup, C) => C)                          : C           =   implWrapper.foldRight(z)(op)
  override def forall                     (p: (Tup) => Boolean)                              : Boolean     =   implWrapper forall p
  override def foreach[C]                 (f: (Tup) => C)                                                    { implWrapper.foreach(f) }
  override def minBy[C]                   (f: (Tup) => C)(implicit cmp: Ordering[C])         : Tup         =   implWrapper.minBy(f)
  override def maxBy[C]                   (f: (Tup) => C)(implicit cmp: Ordering[C])         : Tup         =   implWrapper.maxBy(f)
  override def reduce[A1 >: Tup]          (op: (A1, A1) => A1)                               : A1          =   implWrapper reduce op
  override def reduceLeft[B >: Tup]       (op: (B, Tup) => B)                                : B           =   implWrapper reduceLeft op
  override def reduceLeftOption[B >: Tup] (op: (B, Tup) => B)                                : Option[B]   =   implWrapper reduceLeftOption op
  override def reduceOption[A1 >: Tup]    (op: (A1, A1) => A1)                               : Option[A1]  =   implWrapper reduceOption op
  override def reduceRight[B >: Tup]      (op: (Tup, B) => B)                                : B           =   implWrapper reduceRight op
  override def reduceRightOption[B >: Tup](op: (Tup, B) => B)                                : Option[B]   =   implWrapper reduceRightOption op
  override def retain                     (p: (A, B) => Boolean)                             : this.type   = { this.seq foreach { case (k, v) => if (!p(k, v)) this -= k }; this }
  override def transform                  (f: (A, B) => B)                                   : this.type   = { this.iterator foreach { case (k, v) => update(k, f(k, v)) }; this }

  // Collection-morphing methods
  def aIterator : Iterator[A]             = implWrapper.keysIterator
  def aSet      : scala.collection.Set[A] = implWrapper.keySet
  def aValues   : Iterable[A]             = implWrapper.keys

  // Copying methods
  override def copyToArray[C >: Tup] (xs: Array[C], start: Int, len: Int) { implWrapper.copyToArray(xs, start, len) }
  override def copyToBuffer[C >: Tup](dest: collection.mutable.Buffer[C]) { implWrapper.copyToBuffer(dest) }

}
