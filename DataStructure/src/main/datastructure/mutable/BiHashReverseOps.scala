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

  self: BiHashMap[A, B] =>

  type Tup = (B, A)

  private val implWrapper = new BiHashImplWrapper(baMap, abMap)

  def += (ba: (B,  A))(implicit ignore: DummyImplicit)      : this.type   = { put(ba._1, ba._2); this }
  def -= (bKey: B)(implicit ignore: DummyImplicit)          : this.type   = { remove(bKey); this }

  def apply(bKey: B)(implicit ignore: DummyImplicit)        : A           =   implWrapper.apply(bKey)
  def default(bKey: B)(implicit ignore: DummyImplicit)      : A           =   implWrapper.default(bKey)
  def get(bKey: B)(implicit ignore: DummyImplicit)          : Option[A]   =   implWrapper.get(bKey)
  def put(bKey: B, aVal: A)(implicit ignore: DummyImplicit) : Option[A]   =   implWrapper.put(bKey, aVal)
  def remove(bKey: B)(implicit ignore: DummyImplicit)       : Option[A]   =   implWrapper.remove(bKey)
  def update(bKey: B, aVal: A)(implicit ignore: DummyImplicit)              { implWrapper.update(bKey, aVal) }
  def contains(bKey: B)(implicit ignore: DummyImplicit)     : Boolean     =   implWrapper.contains(bKey)

  def andThen[C](k: (A) => C) : PartialFunction[B, C] =   implWrapper andThen k
  def compose[C](g: (C) => B) : (C) => A              =   implWrapper compose g

  def /:[C]          (z: C)(op: (C, Tup) => C)                          : C            =   implWrapper./:(z)(op)
  def /:\[A1 >: Tup] (z: A1)(op: (A1, A1) => A1)                        : A1           =   implWrapper./:\(z)(op)
  def :\[C]          (z: C)(op: (Tup, C) => C)                          : C            =   implWrapper.:\(z)(op)
  def aggregate[C]   (z: C)(seqop: (C, Tup) => C, combop: (C, C) => C)  : C            =   implWrapper.aggregate(z)(seqop, combop)
  def count          (p: (Tup) => Boolean)                              : Int          =   implWrapper count p
  def exists         (p: (Tup) => Boolean)                              : Boolean      =   implWrapper exists p
  def find           (p: (Tup) => Boolean)                              : Option[Tup]  =   implWrapper find p
  def fold[A1 >: Tup](z: A1)(op: (A1, A1) => A1)                        : A1           =   implWrapper.fold(z)(op)
  def foldLeft[C]    (z: C)(op: (C, Tup) => C)                          : C            =   implWrapper.foldLeft(z)(op)
  def foldRight[C]   (z: C)(op: (Tup, C) => C)                          : C            =   implWrapper.foldRight(z)(op)
  def foreach[C]     (f: (Tup) => C)(implicit ignore: DummyImplicit)                     { implWrapper.foreach(f) }
  
  //@ Should `Bijection` have base implementations of these things?  Seems probable
  def bIterator : Iterator[B]             = implWrapper.keysIterator
  def bSet      : scala.collection.Set[B] = implWrapper.keySet
  def bValues   : Iterable[B]             = implWrapper.keys

  //@ Maybe I'll need this; maybe I won't.
  private def forwardize[C](f: (B, A) => C) : (A, B) => C = (a: A, b: B) => f(b, a)

}
