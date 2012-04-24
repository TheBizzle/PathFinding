package datastructure

import collection.mutable.Map


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/12/12
 * Time: 11:33 PM
 */

abstract class Bijection[A, B, M[X, Y] <: Map[X, Y]](protected val abMap: M[A, B], protected val baMap: M[B, A]) extends Map[A, B] {

  override def hashCode() : Int                =   abMap.hashCode() ^ baMap.hashCode()   // XOR the hashcodes of the two maps
  override def clear()                           { abMap.clear(); baMap.clear() }
  override def size       : Int                = { require (abMap.size == baMap.size); abMap.size }

  override def canEqual(other: Any) : Boolean
  override def equals(that: Any)    : Boolean  = {
    that match {
      case b: Bijection[_, _, _] => (b canEqual this) &&
                                    ( (b.abMap.equals(abMap) && b.baMap.equals(baMap)) ||
                                      (b.abMap.equals(baMap) && b.baMap.equals(abMap)) )
      case _                     => false
    }
  }

  // Toggles whether a size map is used to track hash map statistics for the child maps.
  def iterator : Iterator[(A, B)] = abMap.iterator

}
