package datastructure.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] {

  self: BiHashMap[A, B] =>

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

  override def foreach[C](f: ((A, B)) => C) { implWrapper.foreach(f) }

  def aIterator : Iterator[A]             = implWrapper.keysIterator
  def aSet      : scala.collection.Set[A] = implWrapper.keySet
  def aValues   : Iterable[A]             = implWrapper.keys

}
