package datastructure.mutable

import collection.GenTraversableOnce
import collection.mutable.{Map => MMap}
import collection.generic.{FilterMonadic, CanBuildFrom}

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

  private type Tup = (B, A)

  private val implWrapper = new BiHashImplWrapper(baMap, abMap, repr)

  // General manipulation operators
  //@ I'm uneasy about this usage of `BiHashMap` in the return type
  def + [A1 >: A](ba: (B, A1))                              (implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] += ba
  def + [A1 >: A](ba1: (B, A1), ba2: (B, A1), bas: (B, A1)*)(implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] += ba1 += ba2 ++= bas
  def ++[A1 >: A](bas: GenTraversableOnce[(B, A1)])         (implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] ++= bas.seq
  def -          (bKey: A)                                  (implicit ignore: DummyImplicit) : this.type        = clone() -= bKey
  def -          (bKey1: A, bKey2: A, bKeys: A*)            (implicit ignore: DummyImplicit) : this.type        = clone() -= bKey1 -= bKey2 --= bKeys
  def --         (bKeys: GenTraversableOnce[A])             (implicit ignore: DummyImplicit) : this.type        = clone() --= bKeys.seq

  // Satanry
  //@ There is no `That`!
  override def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That], ignore: DummyImplicit) : That = {
    val b = bf(repr)
    if (that.isInstanceOf[collection.IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection
    b.result
  }

  // Self-return operators
  def += (ba: Tup)                      (implicit ignore: DummyImplicit) : this.type = { put(ba._1, ba._2); this }
  def += (ba1: Tup, ba2: Tup, bas: Tup*)(implicit ignore: DummyImplicit) : this.type =   this += ba1 += ba2 ++= bas
  def ++=(bas: TraversableOnce[Tup])    (implicit ignore: DummyImplicit) : this.type = { bas.seq foreach += ; this }
  def -= (bKey: B)                      (implicit ignore: DummyImplicit) : this.type = { remove(bKey); this }
  def -= (bKey1: B, bKey2: B, bKeys: B*)(implicit ignore: DummyImplicit) : this.type = { this -= bKey1; this -= bKey2; this --= bKeys }
  def --=(bas: TraversableOnce[Tup])    (implicit ignore: DummyImplicit) : this.type = { bas.seq foreach -= ; this }

  // General methods
  def apply             (bKey: B)                (implicit ignore: DummyImplicit) : A         =   implWrapper.apply(bKey)
  def contains          (bKey: B)                (implicit ignore: DummyImplicit) : Boolean   =   implWrapper.contains(bKey)
  def default           (bKey: B)                (implicit ignore: DummyImplicit) : A         =   implWrapper.default(bKey)
  def get               (bKey: B)                (implicit ignore: DummyImplicit) : Option[A] =   implWrapper.get(bKey)
  def getOrElse[A1 >: A](bKey: B, default: => A1)(implicit ignore: DummyImplicit) : A1        =   implWrapper.getOrElse(bKey, default)
  def getOrElseUpdate   (bKey: B, aOp: => A)     (implicit ignore: DummyImplicit) : A         =   implWrapper.getOrElseUpdate(bKey, aOp)
  def isDefinedAt       (bKey: B)                (implicit ignore: DummyImplicit) : Boolean   =   implWrapper.isDefinedAt(bKey)
  def put               (bKey: B, aVal: A)       (implicit ignore: DummyImplicit) : Option[A] =   implWrapper.put(bKey, aVal)
  def remove            (bKey: B)                (implicit ignore: DummyImplicit) : Option[A] =   implWrapper.remove(bKey)
  def update            (bKey: B, aVal: A)       (implicit ignore: DummyImplicit)               { implWrapper.update(bKey, aVal) }

  // Function-chaining methods
  def andThen[C]              (k: (A) => C)(implicit ignore: DummyImplicit)                   : PartialFunction[B, C]   = implWrapper andThen k
  def compose[C]              (g: (C) => B)(implicit ignore: DummyImplicit)                   : (C) => A                = implWrapper compose g
  def orElse[B1 <: B, A1 >: A](that: PartialFunction[B1, A1])(implicit ignore: DummyImplicit) : PartialFunction[B1, A1] = implWrapper orElse that

  // Lambda-operation methods
  def /:[C]                      (z: C)(op: (C, Tup) => C)                        (implicit ignore: DummyImplicit) : C           =   implWrapper./:(z)(op)
  def /:\[A1 >: Tup]             (z: A1)(op: (A1, A1) => A1)                      (implicit ignore: DummyImplicit) : A1          =   implWrapper./:\(z)(op)
  def :\[C]                      (z: C)(op: (Tup, C) => C)                        (implicit ignore: DummyImplicit) : C           =   implWrapper.:\(z)(op)
  def aggregate[C]               (z: C)(seqop: (C, Tup) => C, combop: (C, C) => C)(implicit ignore: DummyImplicit) : C           =   implWrapper.aggregate(z)(seqop, combop)
  def collectFirst[B]            (pf: PartialFunction[Tup, B])                    (implicit ignore: DummyImplicit) : Option[B]   =   implWrapper collectFirst pf
  def count                      (p: (Tup) => Boolean)                            (implicit ignore: DummyImplicit) : Int         =   implWrapper count p
  def exists                     (p: (Tup) => Boolean)                            (implicit ignore: DummyImplicit) : Boolean     =   implWrapper exists p
  def find                       (p: (Tup) => Boolean)                            (implicit ignore: DummyImplicit) : Option[Tup] =   implWrapper find p
  def fold[A1 >: Tup]            (z: A1)(op: (A1, A1) => A1)                      (implicit ignore: DummyImplicit) : A1          =   implWrapper.fold(z)(op)
  def foldLeft[C]                (z: C)(op: (C, Tup) => C)                        (implicit ignore: DummyImplicit) : C           =   implWrapper.foldLeft(z)(op)
  def foldRight[C]               (z: C)(op: (Tup, C) => C)                        (implicit ignore: DummyImplicit) : C           =   implWrapper.foldRight(z)(op)
  def forall                     (p: (Tup) => Boolean)                            (implicit ignore: DummyImplicit) : Boolean     =   implWrapper forall p
  def foreach[C]                 (f: (Tup) => C)                                  (implicit ignore: DummyImplicit)                 { implWrapper.foreach(f) }
  def minBy[C]                   (f: (Tup) => C)                (implicit cmp: Ordering[C], ignore: DummyImplicit) : Tup         =   implWrapper.minBy(f)
  def maxBy[C]                   (f: (Tup) => C)                (implicit cmp: Ordering[C], ignore: DummyImplicit) : Tup         =   implWrapper.maxBy(f)
  def reduce[A1 >: Tup]          (op: (A1, A1) => A1)                             (implicit ignore: DummyImplicit) : A1          =   implWrapper reduce op
  def reduceLeft[B >: Tup]       (op: (B, Tup) => B)                              (implicit ignore: DummyImplicit) : B           =   implWrapper reduceLeft op
  def reduceLeftOption[B >: Tup] (op: (B, Tup) => B)                              (implicit ignore: DummyImplicit) : Option[B]   =   implWrapper reduceLeftOption op
  def reduceOption[A1 >: Tup]    (op: (A1, A1) => A1)                             (implicit ignore: DummyImplicit) : Option[A1]  =   implWrapper reduceOption op
  def reduceRight[B >: Tup]      (op: (Tup, B) => B)                              (implicit ignore: DummyImplicit) : B           =   implWrapper reduceRight op
  def reduceRightOption[B >: Tup](op: (Tup, B) => B)                              (implicit ignore: DummyImplicit) : Option[B]   =   implWrapper reduceRightOption op
  def retain                     (p: (A, B) => Boolean)                           (implicit ignore: DummyImplicit) : this.type   = { this.seq foreach { case (k, v) => if (!p(k, v)) this -= k }; this }
  def transform                  (f: (A, B) => B)                                 (implicit ignore: DummyImplicit) : this.type   = { this.iterator foreach { case (k, v) => update(k, f(k, v)) }; this }
  def withDefault                (d: A => B)                                      (implicit ignore: DummyImplicit) : MMap[A, B]  =   implWrapper withDefault d   //@ I'd love to do this with a better return type...

  //@ `Repr` Madness
  type FM = FilterMonadic

  def collect[C, That]    (pf: PartialFunction[Tup, C])    (implicit bf: CanBuildFrom[Repr, C, That],  ignore: DummyImplicit) : That         = implWrapper collect pf
  def filter              (p: Tup => Boolean)                                                (implicit ignore: DummyImplicit) : Repr         = implWrapper filter p
  def flatMap[C, That]    (f: Tup => GenTraversableOnce[C])(implicit bf: CanBuildFrom[Repr, C, That],  ignore: DummyImplicit) : That         = implWrapper flatMap f
  def map[C, That]        (f: Tup => C)                    (implicit bf: CanBuildFrom[Repr, C, That],  ignore: DummyImplicit) : That         = implWrapper map f
  def partition           (p: Tup => Boolean)                                                (implicit ignore: DummyImplicit) : (Repr, Repr) = implWrapper partition p
  def scan[C >: Tup, That](z: C)(op: (C, C) => C)          (implicit cbf: CanBuildFrom[Repr, C, That], ignore: DummyImplicit) : That         = implWrapper.scan(z)(op)
  def scanLeft[C, That]   (z: C)(op: (C, Tup) => C)        (implicit bf: CanBuildFrom[Repr, C, That],  ignore: DummyImplicit) : That         = implWrapper.scanLeft(z)(op)
  def scanRight[C, That]  (z: C)(op: (Tup, C) => C)        (implicit bf: CanBuildFrom[Repr, C, That],  ignore: DummyImplicit) : That         = implWrapper.scanRight(z)(op)
  def span                (p: Tup => Boolean)                                                (implicit ignore: DummyImplicit) : (Repr, Repr) = implWrapper span p
  def withFilter          (p: B => Boolean)                                                  (implicit ignore: DummyImplicit) : FM[B, Repr]  = implWrapper withFilter p

  // Collection-morphing methods
  //@ Should `Bijection` have base implementations of these things?  Seems probable
  def bIterator : Iterator[B]             = implWrapper.keysIterator
  def bSet      : scala.collection.Set[B] = implWrapper.keySet
  def bValues   : Iterable[B]             = implWrapper.keys

  def copyToArray[C >: Tup] (xs: Array[C], start: Int, len: Int)(implicit ignore: DummyImplicit) { implWrapper.copyToArray(xs, start, len) }
  def copyToBuffer[C >: Tup](dest: collection.mutable.Buffer[C])(implicit ignore: DummyImplicit) { implWrapper.copyToBuffer(dest) }

  //@ Maybe I'll need this; maybe I won't.
  private def forwardize[C](f: (B, A) => C) : (A, B) => C = (a: A, b: B) => f(b, a)

}
