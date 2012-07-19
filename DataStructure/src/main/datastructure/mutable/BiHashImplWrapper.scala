package datastructure.mutable

import collection.generic.{FilterMonadic, CanBuildFrom}
import collection.{GenTraversableOnce, Set}
import collection.mutable.{Builder, Map => MMap}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:45 PM
 */

//@ Most of this should probably get abstracted out to a BijectionOpsImplWrapper that gets inherited from this class,
//  and, here, we can override for the few unique behaviors about our particular case (BiHash: `put`, `remove`, `update`)
private[datastructure] class BiHashImplWrapper[X, Y, Repr](primaryMap: MMap[X, Y], secondaryMap: MMap[Y, X], repr: Repr) {

  private type Tup = (X, Y)

  // General methods
  def apply             (key: X)                 : Y           =   primaryMap(key)
  def contains          (key: X)                 : Boolean     =   primaryMap.contains(key)
  def default           (key: X)                 : Y           =   primaryMap.default(key)
  def get               (key: X)                 : Option[Y]   =   primaryMap.get(key)
  def getOrElse[Y1 >: Y](key: X, default: => Y1) : Y1          =   primaryMap.getOrElse(key, default)
  def getOrElseUpdate   (key: X, op: => Y)       : Y           =   primaryMap.getOrElseUpdate(key, op)
  def isDefinedAt       (key: X)                 : Boolean     =   primaryMap.isDefinedAt(key)
  def keys                                       : Iterable[X] =   primaryMap.keys
  def keysIterator                               : Iterator[X] =   primaryMap.keysIterator
  def keySet                                     : Set[X]      =   primaryMap.keySet
  def put               (key: X, value: Y)       : Option[Y]   = { clearBinds(key, value); secondaryMap.put(value, key); primaryMap.put(key, value) }

  // Function-chaining methods
  def andThen[C]              (k: (Y) => C)                   : PartialFunction[X, C]   = primaryMap andThen k
  def compose[C]              (g: (C) => X)                   : (C) => Y                = primaryMap compose g
  def orElse[X1 <: X, Y1 >: Y](that: PartialFunction[X1, Y1]) : PartialFunction[X1, Y1] = primaryMap orElse that

  // Lambda-operation methods
  def /:[C]                      (z: C)(op: (C, Tup) => C)                         : C           =   primaryMap./:(z)(op)
  def /:\[C >: Tup]              (z: C)(op: (C, C) => C)                           : C           =   primaryMap./:\(z)(op)
  def :\[C]                      (z: C)(op: (Tup, C) => C)                         : C           =   primaryMap.:\(z)(op)
  def aggregate[C]               (z: C)(seqop: (C, Tup) => C, combop: (C, C) => C) : C           =   primaryMap.aggregate(z)(seqop, combop)
  def collectFirst[B]            (pf: PartialFunction[Tup, B])                     : Option[B]   =   primaryMap collectFirst pf
  def count                      (p: (Tup) => Boolean)                             : Int         =   primaryMap count p
  def exists                     (p: (Tup) => Boolean)                             : Boolean     =   primaryMap exists p
  def find                       (p: (Tup) => Boolean)                             : Option[Tup] =   primaryMap find p
  def fold[C >: Tup]             (z: C)(op: (C, C) => C)                           : C           =   primaryMap.fold(z)(op)
  def foldLeft[C]                (z: C)(op: (C, Tup) => C)                         : C           =   primaryMap.foldLeft(z)(op)
  def foldRight[C]               (z: C)(op: (Tup, C) => C)                         : C           =   primaryMap.foldRight(z)(op)
  def forall                     (p: (Tup) => Boolean)                             : Boolean     =   primaryMap forall p
  def foreach[C]                 (f: (Tup) => C)                                                   { primaryMap foreach f }
  def minBy[C]                   (f: (Tup) => C)(implicit cmp: Ordering[C])        : Tup         =   primaryMap minBy f
  def maxBy[C]                   (f: (Tup) => C)(implicit cmp: Ordering[C])        : Tup         =   primaryMap maxBy f
  def reduce[C >: Tup]           (op: (C, C) => C)                                 : C           =   primaryMap reduce op
  def reduceLeft[C >: Tup]       (op: (C, Tup) => C)                               : C           =   primaryMap reduceLeft op
  def reduceLeftOption[C >: Tup] (op: (C, Tup) => C)                               : Option[C]   =   primaryMap reduceLeftOption op
  def reduceOption[C >: Tup]     (op: (C, C) => C)                                 : Option[C]   =   primaryMap reduceOption op
  def reduceRight[C >: Tup]      (op: (Tup, C) => C)                               : C           =   primaryMap reduceRight op
  def reduceRightOption[C >: Tup](op: (Tup, C) => C)                               : Option[C]   =   primaryMap reduceRightOption op
  def withDefault                (d: X => Y)                                       : MMap[X, Y]  =   primaryMap withDefault d   //@ I'd love to do this with a better return type...

  //@ `Repr` Madness
  private type FM = FilterMonadic

  def collect[C, That]    (pf: PartialFunction[Tup, C])    (implicit bf: CanBuildFrom[Repr, C, That])  : That         =   reprMadness(primaryMap collect pf)
  def filter              (p: Tup => Boolean)                                                          : Repr         =   reprMadness(primaryMap filter p)
  def flatMap[C, That]    (f: Tup => GenTraversableOnce[C])(implicit bf: CanBuildFrom[Repr, C, That])  : That         =   reprMadness(primaryMap flatMap f)
  def map[C, That]        (f: Tup => C)                    (implicit bf: CanBuildFrom[Repr, C, That])  : That         =   reprMadness(primaryMap map f)
  def partition           (p: Tup => Boolean)                                                          : (Repr, Repr) = { val (a, b) = primaryMap partition p; (reprMadness(a), reprMadness(b)) }
  def scan[C >: Tup, That](z: C)(op: (C, C) => C)          (implicit cbf: CanBuildFrom[Repr, C, That]) : That         =   reprMadness(primaryMap.scan(z)(op))
  def scanLeft[C, That]   (z: C)(op: (C, Tup) => C)        (implicit bf: CanBuildFrom[Repr, C, That])  : That         =   reprMadness(primaryMap.scanLeft(z)(op))
  def scanRight[C, That]  (z: C)(op: (Tup, C) => C)        (implicit bf: CanBuildFrom[Repr, C, That])  : That         =   reprMadness(primaryMap.scanRight(z)(op))
  def span                (p: Tup => Boolean)                                                          : (Repr, Repr) = { val (a, b) = primaryMap span p; (reprMadness(a), reprMadness(b)) }
  def withFilter          (p: X => Boolean)                                                            : FM[X, Repr]  =   new WithFilter(p)

  // Miscellaneously-used methods
  def copyToArray[C >: Tup] (xs: Array[C], start: Int = 0, len: Int = primaryMap.size) { primaryMap.copyToArray(xs, start, len) }
  def copyToBuffer[C >: Tup](dest: collection.mutable.Buffer[C])                       { primaryMap.copyToBuffer(dest) }

  def remove(key: X) : Option[Y] = {
    get(key) foreach (secondaryMap.remove(_))
    primaryMap.remove(key)
  }

  def update(x: X, y: Y) {
    val hold = primaryMap.get(x)
    primaryMap.update(x, y)
    hold foreach (secondaryMap.remove(_))
    secondaryMap.put(y, x)
  }

  private def clearBinds(x: X, y: Y) {
    def removeY(y: Y) {
      secondaryMap.get(y) foreach (primaryMap.remove(_))
      secondaryMap.remove(y)
    }
    removeY(y); remove(x)
  }

  //@ Size hints got abandoned, since using them would make the design into a yucky mess.  They likely won't be missed....
  private def reprMadness[C, That](that: Iterable[C])(implicit bf: CanBuildFrom[Repr, C, That]) : That = { val b = bf(repr); b ++= that; b.result() }

}
