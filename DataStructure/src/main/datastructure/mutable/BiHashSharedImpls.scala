package datastructure.mutable

import datastructure.Bijection

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:45 PM
 */

trait BiHashSharedImpls[A, B] {

  self: FutureBiHashMap[A, B] with Bijection[A, B] =>

  def put_base(a: A, b: B) : (Option[A], Option[B]) = {
    val aOpt = baMap.put(b, a)
    val bOpt = abMap.put(a, b)
    (aOpt, bOpt)
  }

  def remove_base(aKey: A) : Option[B] = {
    abMap.get(aKey) foreach (baMap.remove(_))
    abMap.remove(aKey)
  }

  def update_base(a: A, b: B) {
    val hold = abMap.get(a)
    abMap.update(a, b)
    hold foreach (baMap.remove(_))
    baMap.put(b, a)
  }

}
