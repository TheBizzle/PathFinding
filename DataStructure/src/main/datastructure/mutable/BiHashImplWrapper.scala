package datastructure.mutable

import collection.mutable.Map

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:45 PM
 */

private[datastructure] class BiHashImplWrapper[X, Y](primaryMap: Map[X, Y], secondaryMap: Map[Y, X]) {

  def apply(key: X)         : Y           =   primaryMap(key)
  def default(key: X)       : Y           =   primaryMap.default(key)
  def get(key: X)           : Option[Y]   =   primaryMap.get(key)
  def put(xKey: X, yVal: Y) : Option[Y]   = { clearBinds(xKey, yVal); secondaryMap.put(yVal, xKey); primaryMap.put(xKey, yVal) }
  def contains(key: X)      : Boolean     =   primaryMap.contains(key)
  def keysIterator          : Iterator[X] =   primaryMap.keysIterator
  def keySet    : scala.collection.Set[X] =   primaryMap.keySet
  def foreach[C](f: ((X, Y)) => C)          { primaryMap foreach f }

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
