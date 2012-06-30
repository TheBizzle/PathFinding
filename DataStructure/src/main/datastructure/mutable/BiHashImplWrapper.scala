package datastructure.mutable

import collection.mutable.Map
import collection.Set

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:45 PM
 */

private[datastructure] class BiHashImplWrapper[X, Y](primaryMap: Map[X, Y], secondaryMap: Map[Y, X]) {

  type Tup = (X, Y)

  def apply       (key: X)           : Y           =   primaryMap(key)
  def contains    (key: X)           : Boolean     =   primaryMap.contains(key)
  def default     (key: X)           : Y           =   primaryMap.default(key)
  def get         (key: X)           : Option[Y]   =   primaryMap.get(key)
  def keys                           : Iterable[X] =   primaryMap.keys
  def keysIterator                   : Iterator[X] =   primaryMap.keysIterator
  def keySet                         : Set[X]      =   primaryMap.keySet
  def put         (xKey: X, yVal: Y) : Option[Y]   = { clearBinds(xKey, yVal); secondaryMap.put(yVal, xKey); primaryMap.put(xKey, yVal) }

  def andThen[C](k: (Y) => C) : PartialFunction[X, C] = primaryMap andThen k
  def compose[C](g: (C) => X) : (C) => Y              = primaryMap compose g

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
  def foreach[C]     (f: (Tup) => C)                                                   { primaryMap foreach f }
  // filter, map, flatMap, collect, collectFirst (`Repr`s...)

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
