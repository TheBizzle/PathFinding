package datastructure.mutable

import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:47 PM
 */

trait BiHashForwardOps[A, B] extends BijectionForwardOps[A, B, HashMap, BiHashMap] {
  self: Bijection[A, B, HashMap, BiHashMap] =>
}


