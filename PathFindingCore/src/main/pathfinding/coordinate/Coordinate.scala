package pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 12:25 AM
 */

case class Coordinate(x: Int = Coordinate.InvalidValue, y: Int = Coordinate.InvalidValue) {

    override def toString : String = {
        "(" + x + "," + y + ")"
    }

    override def clone() : Coordinate = {
        Coordinate(x, y)
    }

    /**
     * This is the same as equals() HERE IN THE COORDINATE CLASS, but subclasses will
     * have their own implementations of equals() without their definition of overlappingness
     * changing, so a universal method for determining overlappingness of of all Coordinates
     * should be defined here.
     */
    def overlaps(that: Any) : Boolean = {
        that match {
            case thatCoord: Coordinate => (x == thatCoord.x) && (y == thatCoord.y)
            case _                     => false
        }
    }

    override def equals(that: Any) : Boolean = {
        that match {
            case thatCoord: Coordinate => (thatCoord canEqual this) && (x == thatCoord.x) && (y == thatCoord.y)
            case _                     => false
        }
    }

    override def hashCode : Int = {
        41 * (4111 + x) + y
    }

    def canEqual(that: Any) : Boolean = {
        that.isInstanceOf[Coordinate]
    }

}

object Coordinate {
    val InvalidValue = -1    // Invalid (initial) X/Y values for Coordinates
}
