package astar_base

import datastructure.priorityqueue.PriorityQueue
import pathfinding.coordinate.{ Coordinate2D, PriorityCoordinate }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/14/12
 * Time: 8:01 PM
 */

trait AStarGruntwork[T <: AStarStepData] {

  self: AStarBase[T] =>

  protected def calculateMaxIters(colCount: Int, rowCount: Int) = (colCount * rowCount * scalingFactor).floor.toInt

  /**
   * Some serious spaghetti!
   * Returns the first location in "queue" that hasn't already been examined (as determined by checking "beenThere").
   */
  //@ Make some type variables (and maybe implicits) to carry about for "Coordinate2D with PriorityCoordinate"
  protected def getFreshLoc(queue: PriorityQueue[Coordinate2D with PriorityCoordinate], beenThere: Array[Array[Boolean]]) : Option[Coordinate2D with PriorityCoordinate] = {
    val loc = queue.dequeue()
    if (!beenThere(loc.x)(loc.y))
      Option(loc) // Exit point (success)
    else {
      if (!queue.isEmpty)
        getFreshLoc(queue, beenThere)
      else
        None // Exit point (failure)
    }
  }

}
