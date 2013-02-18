package org.bizzle.pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 12:25 AM
 */

sealed trait Coordinate extends Equals {

  self: DimensionSet =>

  protected type CoordType <: Coordinate

  def copy : CoordType

  def isValid = true
  def overlaps(that: Coordinate with DimensionSet) = that.isInstanceOf[CoordType] && this.axisCollection == that.axisCollection

  override def toString = axisCollection.mkString("(", ",", ")")
  override def hashCode = {
    axisCollection.foldLeft((4111, true)) {
      case ((hash, isFirst), x) =>
        val h = if (isFirst) hash else 41 * hash
        (h + x, false)
    }._1
  }

  override def canEqual(that: Any) = that.isInstanceOf[CoordType]
  override def equals  (that: Any) = {
    that match {
      case thatCoord: Coordinate => (thatCoord canEqual this) && (thatCoord overlaps this)
      case _                     => false
    }
  }

}

trait DimensionSet {
  def axisCollection = Seq[Int]()
}

trait OneD extends DimensionSet {
  def x: Int
  override def axisCollection = super.axisCollection :+ x
}

trait TwoD extends OneD {
  def y: Int
  override def axisCollection = super.axisCollection :+ y
}

trait ThreeD extends TwoD {
  def z: Int
  override def axisCollection = super.axisCollection :+ z
}

// Boy... I think I see why `TupleN` is such a pile of crap...
trait Coordinate1D extends Coordinate with OneD {
  override protected type CoordType = Coordinate1D
  override def copy = Coordinate(x)
}

trait Coordinate2D extends Coordinate with TwoD {
  override protected type CoordType = Coordinate2D
  override def copy = Coordinate(x, y)
}

trait Coordinate3D extends Coordinate with ThreeD {
  override protected type CoordType = Coordinate3D
  override def copy = Coordinate(x, y, z)
}

object BadCoordinate2D extends Coordinate2D with BadCoordinate {
  override val (x, y) = (-1, -1)
}

object Coordinate {
  def apply(_x: Int)                   = new Coordinate1D { override val  x        =  _x }
  def apply(_x: Int, _y: Int)          = new Coordinate2D { override val (x, y)    = (_x, _y) }
  def apply(_x: Int, _y: Int, _z: Int) = new Coordinate3D { override val (x, y, z) = (_x, _y, _z) }
}

//@ This can work really nicely with dynamic mixins; can do the same kind of thing with mixing in `PriorityCoordinate`, as well
sealed trait BadCoordinate {
  self: Coordinate =>
  override def isValid                                       = false
  override def overlaps(that: Coordinate with DimensionSet)  = false
  override def canEqual(other: Any)                          = other.isInstanceOf[CoordType with BadCoordinate]
}
