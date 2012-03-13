package pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/5/11
 * Time: 2:17 AM
 */

object PriorityCoordinateOrdering extends Ordering[PriorityCoordinate] {
  def compare(a: PriorityCoordinate, b: PriorityCoordinate) = {
    b.priority.compare(a.priority)
  }
}
