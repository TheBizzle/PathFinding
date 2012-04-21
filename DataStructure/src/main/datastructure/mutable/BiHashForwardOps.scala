package datastructure.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] extends BiHashSharedImpls[A, B] {

  self: FutureBiHashMap[A, B] =>

  //@ Many of these will get overridden
  def apply(aKey: A) : B = abMap(aKey)
  def default(aKey: A) : B = throw new NoSuchElementException("key not found: " + aKey)
  def get(aKey: A) : Option[B] = abMap.get(aKey)
  def += (ab: (A,  B)) : this.type = { put(ab._1, ab._2); this }
  def put(aKey: A, bVal: B) : Option[A] = put_base(aKey, bVal)._1
  def remove(aKey: A) : Option[B] = remove_base(aKey)
  def -= (aKey: A) : this.type = { remove(aKey); this }
  def update(aKey: A, bVal: B) { update_base(aKey, bVal) }
  def contains(aKey: A) : Boolean = abMap.contains(aKey)
  
}
