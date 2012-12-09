package datastructure.mutable

import collection.{ CustomParallelizable, generic, GenTraversableOnce, mutable }
import mutable.{ HashMap, Map => MMap, MapLike }
import generic.{ CanBuildFrom, FilterMonadic, MutableMapFactory }

import datastructure.Bijection
import datastructure.parallel.mutable.ParBiHashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/18/12
 * Time: 11:15 PM
 */

//@ Should probably implement `toString` correctly; currently claims this to be a `Map`
//@ Should I instantiate and fill the maps here, or should I instantiate here and leave the population of the maps to `Bijection`...?
//@ Should add `<->` as a tuple-building operator
class BiHashMap[A, B] private[datastructure](abm: HashMap[A, B], bam: HashMap[B, A])
    extends Bijection[A, B, HashMap, HashMap[A, B], HashMap[B, A], BiHashMap[A, B]](abm, bam)
    with MapLike[A, B, BiHashMap[A, B]]
    with CustomParallelizable[(A, B), ParBiHashMap[A, B]]
    with BiHashForwardOps[A, B]
    with BiHashReverseOps[A, B] {

  private type Tup = (A, B)

  def this(contents: (A, B)*)(implicit aToBMap: HashMap[A, B] = contents.foldRight(new HashMap[A, B]){ case (ab, m) => m += ab },
                                       bToAMap: HashMap[B, A] = contents.foldRight(new HashMap[B, A]){ case (ab, m) => m += ab._2 -> ab._1 }) {
    this(aToBMap, bToAMap)
  }

  // Toggles whether a size map is used to track hash map statistics for the child maps.
  def useSizeMap(t: Boolean)                     { abMap.useSizeMap(t); baMap.useSizeMap(t) }
  def swap                   : BiHashMap[B, A] =   new BiHashMap(baMap.clone(), abMap.clone())
  def flip                   : BiHashMap[B, A] =   swap                                        // I feel like I should get rid of one, but I like both names...

  // Lambda-operation methods
  //@ At least some of these should probably be moved to `Bijection`
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

  override def clone() : BiHashMap[A, B]      = new BiHashMap(abMap.toSeq: _*)
  override def par     : ParBiHashMap[A, B]   = throw new UnsupportedOperationException("`ParBiHashMap` is not yet in an operable state.") //@
  override def empty   : BiHashMap[A, B]      = BiHashMap.empty[A, B]
  override def canEqual(other: Any) : Boolean = other.isInstanceOf[BiHashMap[A, B]]  // Might pay to do "|| other.isInstanceOf[BiHashMap[B, A]]"... if not for type erasure

}

object BiHashMap extends MutableMapFactory[BiHashMap] {
  implicit def canBuildFrom[A, B] : CanBuildFrom[Coll, (A, B), BiHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B] : BiHashMap[A, B] = new BiHashMap[A, B]()
}
