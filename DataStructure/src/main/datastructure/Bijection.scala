package datastructure

import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/12/12
 * Time: 11:33 PM
 */

abstract class Bijection[A, B](contents: (A, B)*)(implicit aToBMap: HashMap[A, B] = contents.foldRight(new HashMap[A, B]){ case (ab, m) => m += ab },
                                                           bToAMap: HashMap[B, A] = contents.foldRight(new HashMap[B, A]){ case (ab, m) => m += ab._2 -> ab._1 }) {
  protected val abMap = aToBMap
  protected val baMap = bToAMap
}
