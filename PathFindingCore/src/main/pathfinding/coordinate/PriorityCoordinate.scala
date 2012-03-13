package pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/5/11
 * Time: 1:52 AM
 */

case class PriorityCoordinate(private val xLoc: Int, private val yLoc: Int, priority: Int) extends Coordinate(xLoc, yLoc) {

  def this(coord: Coordinate, priorityVal: Int) {
    this(coord.x, coord.y, priorityVal)
  }

  def asCoordinate : Coordinate = {
    Coordinate(x, y)
  }

  override def toString : String = {
    super.toString + ":" + priority
  }

  override def clone() : PriorityCoordinate = {
    PriorityCoordinate(x, y, priority)
  }

  override def equals(that: Any) : Boolean = {
    that match {
      case thatCoord: PriorityCoordinate => super.equals(that) && (priority == thatCoord.priority)
      case _                             => false
    }
  }

  override def hashCode : Int = {
    41 * super.hashCode + priority
  }

  override def canEqual(other: Any) : Boolean = {
    other.isInstanceOf[PriorityCoordinate]
  }

}
