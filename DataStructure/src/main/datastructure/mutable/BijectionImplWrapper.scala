package datastructure.mutable

import
  collection.{ mutable, Set },
    mutable.{ Map => MMap }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:45 PM
 */

private[mutable] class BijectionImplWrapper[X, Y, M[I, J] <: MMap[I, J]](primaryMap: M[X, Y], secondaryMap: M[Y, X]) {

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

  def put(key: X, value: Y) : Option[Y] = {
    val x = primaryMap.get(key)
    clearBinds(key, value)
    secondaryMap.put(value, key)
    primaryMap.put(key, value)
    x
  }

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

  // Function-chaining methods
  def andThen[C]              (k: (Y) => C)                   : PartialFunction[X, C]   = primaryMap andThen k
  def compose[C]              (g: (C) => X)                   : (C) => Y                = primaryMap compose g
  def orElse[X1 <: X, Y1 >: Y](that: PartialFunction[X1, Y1]) : PartialFunction[X1, Y1] = primaryMap orElse that

  def filterKeys(p: (X) => Boolean) : collection.Map[X, Y] = primaryMap filterKeys p
  def mapValues[C](f: (Y) => C)     : collection.Map[X, C] = primaryMap mapValues f

  // Miscellaneously-used methods
  def copyToArray[C >: Tup] (xs: Array[C])                                             { primaryMap.copyToArray(xs) }
  def copyToArray[C >: Tup] (xs: Array[C], start: Int)                                 { primaryMap.copyToArray(xs, start) }
  def copyToArray[C >: Tup] (xs: Array[C], start: Int = 0, len: Int = primaryMap.size) { primaryMap.copyToArray(xs, start, len) }
  def copyToBuffer[C >: Tup](dest: collection.mutable.Buffer[C])                       { primaryMap.copyToBuffer(dest) }

  private def clearBinds(x: X, y: Y) {
    def removeY(y: Y) {
      secondaryMap.get(y) foreach (primaryMap.remove(_))
      secondaryMap.remove(y)
    }
    removeY(y); remove(x)
  }

}
