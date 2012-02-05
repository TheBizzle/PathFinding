package pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/5/11
 * Time: 1:52 AM
 */

class PriorityCoordinate(xLoc: Int, yLoc: Int, priorityVal: Int) extends Coordinate(xLoc, yLoc) {

    val priority = priorityVal

    def this(coord: Coordinate, priorityVal: Int) {
        this(coord.x, coord.y, priorityVal)
    }

    def asCoordinate : Coordinate = {
        new Coordinate(x, y)
    }

    override def toString : String = {
         super.toString + ":" + priority
    }

    override def clone() : PriorityCoordinate = {
        new PriorityCoordinate(x, y, priority)
    }

    override def equals(that: Any) : Boolean = {
        that match {
            case thatCoord: PriorityCoordinate => super.equals(that) && (priority == thatCoord.priority)
            case _ => false
        }
    }

    override def hashCode : Int = {
        41 * super.hashCode + priority
    }

    override def canEqual(other: Any) : Boolean = {
        other.isInstanceOf[PriorityCoordinate]
    }

}
