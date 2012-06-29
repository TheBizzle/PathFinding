package datastructure.mutable

import collection.generic.CanBuildFrom

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] {

  self: BiHashMap[A, B] =>

  type Tup = (A, B)

  private val implWrapper = new BiHashImplWrapper(abMap, baMap)

  override def += (ab: (A,  B))      : this.type   = { put(ab._1, ab._2); this }
  override def -= (aKey: A)          : this.type   = { remove(aKey); this }

  override def apply(aKey: A)        : B           =   implWrapper.apply(aKey)
  override def default(aKey: A)      : B           =   implWrapper.default(aKey)
  override def get(aKey: A)          : Option[B]   =   implWrapper.get(aKey)
  override def put(aKey: A, bVal: B) : Option[B]   =   implWrapper.put(aKey, bVal)
  override def remove(aKey: A)       : Option[B]   =   implWrapper.remove(aKey)
  override def update(aKey: A, bVal: B)              { implWrapper.update(aKey, bVal) }
  override def contains(aKey: A)     : Boolean     =   implWrapper.contains(aKey)

  override def andThen[C](k: (B) => C) : PartialFunction[A, C] =   implWrapper andThen k
  override def compose[C](g: (C) => A) : (C) => B =                implWrapper compose g

  //@ Am I doing the first type parameter for these `CanBuildFrom`s correctly...?
  // override def collect[C, That](pf: PartialFunction[(A, B), C])(implicit bf: CanBuildFrom[this.type, C, That]) : That = implWrapper.collect(pf)

  override def /:[C]          (z: C)(op: (C, Tup) => C)   : C           =   implWrapper./:(z)(op)
  override def /:\[A1 >: Tup] (z: A1)(op: (A1, A1) => A1) : A1          =   implWrapper./:\(z)(op)
  override def :\[C]          (z: C)(op: (Tup, C) => C)   : C           =   implWrapper.:\(z)(op)
  override def aggregate[C]   (z: C)(seqop: (C, Tup) => C,
                                     combop: (C, C) => C) : C           =   implWrapper.aggregate(z)(seqop, combop)
  override def count          (p: (Tup) => Boolean)       : Int         =   implWrapper count p
  override def exists         (p: (Tup) => Boolean)       : Boolean     =   implWrapper exists p
  override def find           (p: (Tup) => Boolean)       : Option[Tup] =   implWrapper find p
  override def fold[A1 >: Tup](z: A1)(op: (A1, A1) => A1) : A1          =   implWrapper.fold(z)(op)
  override def foldLeft[C]    (z: C)(op: (C, Tup) => C)   : C           =   implWrapper.foldLeft(z)(op)
  override def foldRight[C]   (z: C)(op: (Tup, C) => C)   : C           =   implWrapper.foldRight(z)(op)
  override def foreach[C]     (f: (Tup) => C)                             { implWrapper foreach f }

  def aIterator : Iterator[A]             = implWrapper.keysIterator
  def aSet      : scala.collection.Set[A] = implWrapper.keySet
  def aValues   : Iterable[A]             = implWrapper.keys

}
