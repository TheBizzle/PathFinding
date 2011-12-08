package astar_base

import datastructure.priorityqueue.PriorityQueue
import java.security.InvalidParameterException
import coordinate.{Coordinate, PriorityCoordinate}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:04 PM
 */

trait AStarLike[T >: StepData] {

    self:AStarBase[T] =>

    protected def calculateMaxIters(colCount: Int, rowCount: Int) : Int = {
        math.floor(colCount * rowCount * scalingFactor).toInt
    }

    /**
     * Some serious spaghetti!
     * Returns the first location in "queue" that hasn't already been examined (as determined by checking "beenThere").
     */
    protected def getFreshLoc(queue: PriorityQueue[PriorityCoordinate], beenThere: Array[Array[Boolean]]): PriorityCoordinate = {
        queue.dequeue() match {
            case None => throw new InvalidParameterException
            case Some(loc) => {
                if (beenThere(loc.x)(loc.y)) {
                    if (!queue.isEmpty)
                        getFreshLoc(queue, beenThere)
                    else
                        throw new InvalidParameterException
                }
                else
                    loc     // Exit point (success)
            }
        }
    }

    protected def initialize2DArr[T: Manifest](cols: Int, rows: Int, defaultVal: T) : Array[Array[T]] = {
        new Array[Array[T]](cols) map ( x => new Array[T](rows) map (y => defaultVal) )
    }

    protected def queueDoesContain(coord: Coordinate, queue: PriorityQueue[PriorityCoordinate]) : Boolean = {
        queue.foreach { case(x) => if (x == coord) true }
        false
    }

}