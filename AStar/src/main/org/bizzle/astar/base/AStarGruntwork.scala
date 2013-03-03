package org.bizzle.astar.base

import
  scala.{ annotation, collection },
    annotation.tailrec,
    collection.mutable.PriorityQueue

import
  org.bizzle.pathfinding.coordinate.{ Coordinate2D, PriorityCoordinate }

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
  @tailrec // The side-effecty nature of this actually makes it quite difficult to abstract away the tail recursion into a loop or typical library call
  protected final def getFreshLoc(queue: PriorityQueue[PFCoord], beenThere: Array[Array[Boolean]]) : Option[PFCoord] = {
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
