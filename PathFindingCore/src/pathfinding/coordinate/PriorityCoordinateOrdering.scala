package pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/5/11
 * Time: 2:17 AM
 */

object PriorityCoordinateOrdering extends Ordering[PriorityCoordinate] {
    def compare(a: PriorityCoordinate, b: PriorityCoordinate) = {

        val ap = a.priority
        val bp = b.priority

        if (ap == bp)
            0
        else if (ap < bp)
            1
        else
            -1

    }
}
