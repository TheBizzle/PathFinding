package astar_base

import datastructure.priorityqueue.PriorityQueue
import exceptions.UnexpectedDataException
import pathfinding.coordinate.{Coordinate, PriorityCoordinate}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:04 PM
 */

trait AStarLike[T <: AStarStepData] {

    self: AStarBase[T] =>

    protected def calculateMaxIters(colCount: Int, rowCount: Int) : Int = {
        (colCount * rowCount * scalingFactor).floor.toInt
    }

    /**
     * Some serious spaghetti!
     * Returns the first location in "queue" that hasn't already been examined (as determined by checking "beenThere").
     */
    protected def getFreshLoc(queue: PriorityQueue[PriorityCoordinate], beenThere: Array[Array[Boolean]]) : Option[PriorityCoordinate] = {
        queue.dequeue() match {
            case None => throw new UnexpectedDataException("Popped a None off the queue!")
            case Some(loc) => {
                if (beenThere(loc.x)(loc.y)) {
                    if (!queue.isEmpty)
                        getFreshLoc(queue, beenThere)
                    else
                        None      // Exit point (failure)
                }
                else
                    Some(loc)     // Exit point (success)
            }
        }
    }

    protected def initialize2DArr[T: Manifest](cols: Int, rows: Int, defaultVal: T) : Array[Array[T]] = {
        new Array[Array[T]](cols) map ( x => new Array[T](rows) map (y => defaultVal) )
    }

    protected def queueDoesContain(coord: Coordinate, queue: PriorityQueue[PriorityCoordinate]) : Boolean = {
        queue.foreach { case(x) => if (coord overlaps x) true }
        false
    }

}