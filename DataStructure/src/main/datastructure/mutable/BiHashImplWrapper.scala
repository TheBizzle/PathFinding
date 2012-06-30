package datastructure.mutable

import collection.mutable.Map
import collection.Set

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:45 PM
 */

private[datastructure] class BiHashImplWrapper[X, Y, Repr](primaryMap: Map[X, Y], secondaryMap: Map[Y, X]) {

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
  def /:[C]          (z: C)(op: (C, Tup) => C)                         : C           =   primaryMap./:(z)(op)
  def /:\[A1 >: Tup] (z: A1)(op: (A1, A1) => A1)                       : A1          =   primaryMap./:\(z)(op)
  def :\[C]          (z: C)(op: (Tup, C) => C)                         : C           =   primaryMap.:\(z)(op)
  def aggregate[C]   (z: C)(seqop: (C, Tup) => C, combop: (C, C) => C) : C           =   primaryMap.aggregate(z)(seqop, combop)
  def count          (p: (Tup) => Boolean)                             : Int         =   primaryMap count p
  def exists         (p: (Tup) => Boolean)                             : Boolean     =   primaryMap exists p
  def find           (p: (Tup) => Boolean)                             : Option[Tup] =   primaryMap find p
  def fold[A1 >: Tup](z: A1)(op: (A1, A1) => A1)                       : A1          =   primaryMap.fold(z)(op)
  def foldLeft[C]    (z: C)(op: (C, Tup) => C)                         : C           =   primaryMap.foldLeft(z)(op)
  def foldRight[C]   (z: C)(op: (Tup, C) => C)                         : C           =   primaryMap.foldRight(z)(op)
  def forall         (p: (Tup) => Boolean)                             : Boolean     =   primaryMap forall p
  def foreach[C]     (f: (Tup) => C)                                                   { primaryMap foreach f }
  def minBy[C]       (f: (Tup) => C)(implicit cmp: Ordering[C])        : Tup         =   primaryMap minBy f
  def maxBy[C]       (f: (Tup) => C)(implicit cmp: Ordering[C])        : Tup         =   primaryMap maxBy f

  
  //@ `Repr`s:
  // filter, map, mapResult, flatMap, collect, collectFirst, partition, withDefault, withFilter

  //@ Normals:
  // copyToArray x 3, copyToBuffer
  // reduce, reduceOption, reduceLeft, reduceLeftOption, reduceRight, reduceRightOption, retain, sameElements
  // scan, scanLeft, scanRight, span, transform, updated


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

}
