package datastructure.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:41 PM
 */

trait BiHashReverseOps[A, B] extends BiHashSharedImpls[A, B] {

  self: FutureBiHashMap[A, B] with BiHashForwardOps[A, B] =>

  import ErasureDefeater._

  def apply(bKey: B)(implicit ignore: ErasureDefeater = BHMED)        : A =           baMap(bKey)
  def default(bKey: B)(implicit ignore: ErasureDefeater = BHMED)      : B =           throw new NoSuchElementException("key not found: " + bKey)
  def get(bKey: B)(implicit ignore: ErasureDefeater = BHMED)          : Option[A] =   baMap.get(bKey)
  def += (ba: (B,  A))(implicit ignore: ErasureDefeater = BHMED)      : this.type = { put(ba._1, ba._2); this }
  def put(bKey: B, aVal: A)(implicit ignore: ErasureDefeater = BHMED) : Option[A] =   put_base(aVal, bKey)._1
  def remove(bKey: B)(implicit ignore: ErasureDefeater = BHMED)       : Option[A] = { val holdOn = baMap.get(bKey); holdOn foreach (remove_base(_)); holdOn }
  def -= (bKey: B)(implicit ignore: ErasureDefeater = BHMED)          : this.type = { remove(bKey); this }
  def update(bKey: B, aVal: A)(implicit ignore: ErasureDefeater = BHMED)            { update_base(aVal, bKey) }
  def contains(bKey: B)(implicit ignore: ErasureDefeater = BHMED)     : Boolean =     baMap.contains(bKey)

}

// The `BHMED` ("BiHashMapErasureDefeater") is tacked onto each method here to give a different signature to the JVM than the one in `_ForwardOps` has
sealed trait ErasureDefeater
private object ErasureDefeater {
  object BHMED extends ErasureDefeater
}

