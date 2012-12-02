package pathfinding.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/5/11
 * Time: 2:17 AM
 */

object PriorityCoordinateOrdering extends Ordering[Coordinate2D with PriorityCoordinate] {
  def compare(a: Coordinate2D with PriorityCoordinate, b: Coordinate2D with PriorityCoordinate) = {
    b.priority.compare(a.priority)
  }
}
