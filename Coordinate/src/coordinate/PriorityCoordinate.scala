package coordinate

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

    override def toString : String = {
         super.toString + ":" + priority
    }

    override def equals(that: Any) : Boolean = {
        if (that.isInstanceOf[Coordinate]) {
            val coord = that.asInstanceOf[Coordinate]
            (x == coord.x) && (y == coord.y)
        }
        else
            false
    }

}