package datastructure.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] {

  self: FutureBiHashMap[A, B] =>

  private val implWrapper = new BiHashImplWrapper(abMap, baMap)

  //@ Many of these will get overridden -- Um... what?
  def apply(aKey: A)        : B         =   implWrapper.apply(aKey)
  def default(aKey: A)      : B         =   implWrapper.default(aKey)
  def get(aKey: A)          : Option[B] =   implWrapper.get(aKey)
  def += (ab: (A,  B))      : this.type = { put(ab._1, ab._2); this }
  def put(aKey: A, bVal: B) : Option[B] =   implWrapper.put(aKey, bVal)
  def -= (aKey: A)          : this.type = { remove(aKey); this }
  def remove(aKey: A)       : Option[B] =   implWrapper.remove(aKey)
  def update(aKey: A, bVal: B)            { implWrapper.update(aKey, bVal) }
  def contains(aKey: A)     : Boolean   =   implWrapper.contains(aKey)
  
}
