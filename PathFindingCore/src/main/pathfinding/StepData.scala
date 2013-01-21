package pathfinding

import
  coordinate.{ BadCoordinate2D, Coordinate2D },
  pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 5:27 PM
 */

// `endGoal` is a variable that is useful for pathing algorithms that might have multiple intermediary goals,
// or for things like the backwards-moving agent in bidirectional A*
abstract class StepData(val loc: Coordinate2D, val goal: Coordinate2D, val pathingMap: PathingMap,
                        val breadcrumbArr: Array[Array[Coordinate2D]], val endGoal: Coordinate2D = BadCoordinate2D)

trait StepDataSingleton[T] {
  def apply(freshLoc: Coordinate2D, stepData: T, isIncingIters: Boolean = false) : T
}
