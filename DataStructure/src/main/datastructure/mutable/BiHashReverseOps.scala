package datastructure.mutable

import collection.{ GenTraversableOnce, mutable }, mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/9/12
 * Time: 2:18 PM
 */

trait BiHashReverseOps[A, B] extends BijectionReverseOps[A, B, HashMap, BiHashMap] {

  self: Bijection[A, B, HashMap, BiHashMap] =>

  override def + [A1 >: A](ba: (B, A1))                              (implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] += ba
  override def + [A1 >: A](ba1: (B, A1), ba2: (B, A1), bas: (B, A1)*)(implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] += ba1 += ba2 ++= bas
  override def ++[A1 >: A](bas: GenTraversableOnce[(B, A1)])         (implicit ignore: DummyImplicit) : BiHashMap[A1, B] = clone().asInstanceOf[BiHashMap[A1, B]] ++= bas.seq
  override def -          (bKey: B)                                  (implicit ignore: DummyImplicit) : Repr     = clone() -= bKey
  override def -          (bKey1: B, bKey2: B, bKeys: B*)            (implicit ignore: DummyImplicit) : Repr     = clone() -= bKey1 -= bKey2 --= bKeys
  override def --         (bKeys: GenTraversableOnce[B])             (implicit ignore: DummyImplicit) : Repr     = clone() --= bKeys.seq

}
