package datastructure.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 4/17/12
 * Time: 9:41 PM
 */

// It would be great if there were something that made this task easier--something like `Proxy`s.
// But I'm not aware of any common pattern that allows me to have two different traits inheriting
// the same functionality and wrapping it in different ways, with different type signatures (to avoid type erasure).
// Utilizes `DummyImplicit` (from Predef) to give the methods herein different signatures to the JVM than the `_ForwardsOps` ones get
trait BiHashReverseOps[A, B] {

  self: FutureBiHashMap[A, B] =>

  private val implWrapper = new BiHashImplWrapper(baMap, abMap)

  def apply(bKey: B)(implicit ignore: DummyImplicit)        : A         =   implWrapper.apply(bKey)
  def default(bKey: B)(implicit ignore: DummyImplicit)      : A         =   implWrapper.default(bKey)
  def get(bKey: B)(implicit ignore: DummyImplicit)          : Option[A] =   implWrapper.get(bKey)
  def += (ba: (B,  A))(implicit ignore: DummyImplicit)      : this.type = { put(ba._1, ba._2); this }
  def put(bKey: B, aVal: A)(implicit ignore: DummyImplicit) : Option[A] =   implWrapper.put(bKey, aVal)
  def -= (bKey: B)(implicit ignore: DummyImplicit)          : this.type = { remove(bKey); this }
  def remove(bKey: B)(implicit ignore: DummyImplicit)       : Option[A] =   implWrapper.remove(bKey)
  def update(bKey: B, aVal: A)(implicit ignore: DummyImplicit)            { implWrapper.update(bKey, aVal) }
  def contains(bKey: B)(implicit ignore: DummyImplicit)     : Boolean   =   implWrapper.contains(bKey)

}
