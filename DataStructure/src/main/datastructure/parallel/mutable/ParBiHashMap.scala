package datastructure.parallel
package mutable

/**
* Created by IntelliJ IDEA.
* User: Jason
* Date: 4/16/12
* Time: 10:12 PM
*/

import
  collection.{ generic, GenIterable, GenTraversableOnce, mutable, parallel },
    generic._,
    mutable.{ DefaultEntry, HashMap },
    parallel.{ Combiner, mutable => pmutable },
      pmutable.{ ParHashMap, ParHashTable, ParMapLike, ParMap }

//@ Does not even come close to working right now.  And I do not really care (for the time being).
@SerialVersionUID(1L)
class ParBiHashMap[A, B] private[datastructure](abm: HashMap[A, B], bam: HashMap[B, A])
extends ParHashMap[A, B]
   with ParMap[A, B]
   with GenericParMapTemplate[A, B, ParBiHashMap]
   with ParMapLike[A, B, ParBiHashMap[A, B], datastructure.mutable.BiHashMap[A, B]]
   with ParHashTable[A, DefaultEntry[A, B]] //@ Doubtful
   with Serializable
{
  def this(contents: (A, B)*) = this(new HashMap[A, B](), new HashMap[B, A]()) //@ Fix this... eventually!
  override def mapCompanion: GenericParMapCompanion[ParBiHashMap] = ParBiHashMap
  override def empty: ParBiHashMap[A, B] = new ParBiHashMap[A, B]
  protected[this] override def newCombiner = throw new UnsupportedOperationException("")
  override def size = throw new UnsupportedOperationException("")
  override def scan[Y >: (A, B), That](z: Y)(op: (Y, Y) => Y)(implicit cbf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def scanLeft[Y, That](z: Y)(op: (Y, (A, B)) => Y)(implicit bf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def scanRight[Y, That](z: Y)(op: ((A, B), Y) => Y)(implicit bf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def map[Y, That](f: ((A, B)) => Y)(implicit bf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def get(key: A) = throw new UnsupportedOperationException("")
  override def collect[Y, That](pf: PartialFunction[(A, B), Y])(implicit bf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def flatMap[Y, That](f: ((A, B)) => GenTraversableOnce[Y])(implicit bf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def ++[Y >: (A, B), That](that: GenTraversableOnce[Y])(implicit bf: CanBuildFrom[ParBiHashMap[A, B], Y, That]) = throw new UnsupportedOperationException("")
  override def zip[X1 >: (A, B), Y, That](that: GenIterable[Y])(implicit bf: CanBuildFrom[ParBiHashMap[A, B], (X1, Y), That]) = throw new UnsupportedOperationException("")
  override def zipWithIndex[X1 >: (A, B), That](implicit bf: CanBuildFrom[ParBiHashMap[A, B], (X1, Int), That]) = throw new UnsupportedOperationException("")
  override def zipAll[Y, X1 >: (A, B), That](that: GenIterable[Y], thisElem: X1, thatElem: Y)(implicit bf: CanBuildFrom[ParBiHashMap[A, B], (X1, Y), That]) = throw new UnsupportedOperationException("")
  override def seq = throw new UnsupportedOperationException("")
  override def put(key: A, value: B) = throw new UnsupportedOperationException("")
  override def +=(kv: (A, B)) = throw new UnsupportedOperationException("")
  override def -=(key: A) = throw new UnsupportedOperationException("")
  override def clear() { throw new UnsupportedOperationException("") }
  override def splitter = throw new UnsupportedOperationException("")
}

object ParBiHashMap extends ParMapFactory[ParBiHashMap] {
  var iters = 0
  def empty[A, B]: ParBiHashMap[A, B] = new ParBiHashMap[A, B]
  def newCombiner[A, B]: Combiner[(A, B), ParBiHashMap[A, B]] = throw new UnsupportedOperationException("")
  implicit def canBuildFrom[A, B]: CanCombineFrom[Coll, (A, B), ParBiHashMap[A, B]] = throw new UnsupportedOperationException("")
}

private[parallel] object ParHashMapCombiner {
  private[mutable] val discriminantbits = throw new UnsupportedOperationException("")
  private[mutable] val numblocks = throw new UnsupportedOperationException("")
  private[mutable] val discriminantmask = throw new UnsupportedOperationException("")
  private[mutable] val nonmasklength = throw new UnsupportedOperationException("")

  def apply[A, B] = throw new UnsupportedOperationException("") // was: with EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]]
}

