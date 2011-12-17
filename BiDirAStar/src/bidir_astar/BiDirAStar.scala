package bidir_astar

import concurrency.BiDirDirector
import pathingmap.pathingmapdata.PathingMapString
import astar_base._
import heuristic.{HeuristicBundle, HeuristicLib}
import datastructure.priorityqueue.PriorityQueue
import coordinate._
import pathingmap.PathingMap
import astar_base.statuses._
import java.security.InvalidParameterException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 10:40 PM
 */

// Basically, runs two AStar processes asychronously, and they pass each other their updated beenThere arrays and current locations.
// If one reaches a location that the other has reached, or if the current locations are next to each other, it returns.
object BiDirAStar extends AStarBase[BiDirStepData](0.8, HeuristicLib.manhattanDistance) {

    override def apply(mapString: PathingMapString) : ExecutionStatus[BiDirStepData] = {

        val (start, goal, pathingMap) = PathingMap(mapString)

        val colCount = pathingMap.colCount
        val rowCount = pathingMap.rowCount

        val beenThere = initialize2DArr(colCount, rowCount, false)
        val queue = new PriorityQueue[PriorityCoordinate](PriorityCoordinateOrdering.compare)

        val costArr = initialize2DArr(colCount, rowCount, Coordinate.BadCoordVal)
        val heuristicArr = initialize2DArr(colCount, rowCount, Coordinate.BadCoordVal)
        val totalArr = initialize2DArr(colCount, rowCount, Coordinate.BadCoordVal)
        val breadcrumbArr = initialize2DArr(colCount, rowCount, new Coordinate(Coordinate.BadCoordVal, Coordinate.BadCoordVal))

        costArr(start.x)(start.y) = 0
        heuristicArr(start.x)(start.y) = heuristic(new HeuristicBundle(start, goal))
        totalArr(start.x)(start.y) = costArr(start.x)(start.y) + heuristicArr(start.x)(start.y)

        val otherLoc = goal
        val otherBreadcrumbs = initialize2DArr(colCount, rowCount, new Coordinate(Coordinate.BadCoordVal, Coordinate.BadCoordVal))

        queue.enqueue(new PriorityCoordinate(start, totalArr(start.x)(start.y)))
        execute(new BiDirStepData(start, goal, beenThere, queue, pathingMap,
                                  costArr, heuristicArr, totalArr, breadcrumbArr,
                                  otherLoc, otherBreadcrumbs), maxIters = calculateMaxIters(colCount, rowCount))

    }

    override protected def execute(stepData: BiDirStepData, iters: Int, maxIters: Int) : ExecutionStatus[BiDirStepData] = {
        val director = new BiDirDirector(decide(_: BiDirStepData, _: Int, maxIters), step) // decide() gets partially applied
        director.direct(stepData.clone(), stepData.cloneForBiBackwards(), iters) match {
            case status: ExecutionStatus[BiDirStepData] => status
            case _ => throw new InvalidParameterException
        }
    }

    override protected def decide(stepData: BiDirStepData, iters: Int = 0, maxIters: Int) : ExecutionStatus[BiDirStepData] = {

        import stepData._

        if (!queue.isEmpty && (iters < maxIters)) {

            val freshLoc = getFreshLoc(queue, beenThereArr)
            pathingMap.step(loc, freshLoc)

            // DEBUGGING STATEMENTS
            println(pathingMap.toString + "\n")
            //println("Loc: " + loc)
            //println("Goal: " + destination)
            //println(stepData.queue.toString() + "\n\n")

            val isInBreadcrumbs = othersBreadcrumbs(freshLoc.x)(freshLoc.y) match {
                case coord: Coordinate => (coord.x != Coordinate.BadCoordVal) && (coord.y != Coordinate.BadCoordVal)
                case _ => false
            }

            if (freshLoc overlaps goal || (a @ ) )
                return Success(BiDirStepData(freshLoc, stepData))     // Exit point (success)

            beenThereArr(freshLoc.x)(freshLoc.y) = true
            Continue((BiDirStepData(freshLoc, stepData)))             // Exit point (only to return again soon)

        }
        else
            Failure(BiDirStepData(new Coordinate(Coordinate.BadCoordVal, Coordinate.BadCoordVal), stepData))  // Exit point (failure)

    }

    override protected def step(stepData: BiDirStepData) : BiDirStepData = {

        import stepData._

        pathingMap.neighborsOf(loc).foreach { case(n) => {

            val neighbor = PathingMap.findNeighborCoord(loc, n)
            val x = neighbor.x
            val y = neighbor.y

            if (!beenThereArr(x)(y)) {

                val newCost = costArr(loc.x)(loc.y) + 1
                val doesContainNeighbor = queueDoesContain(neighbor, queue)

                if (!doesContainNeighbor || (newCost < costArr(x)(y))) {
                    costArr(x)(y) = newCost
                    heuristicArr(x)(y) = heuristic(new HeuristicBundle(neighbor, goal))
                    totalArr(x)(y) = costArr(x)(y) + heuristicArr(x)(y)
                    breadcrumbArr(x)(y) = new Coordinate(loc.x, loc.y)
                }

                if (!doesContainNeighbor)
                    queue.enqueue(new PriorityCoordinate(neighbor, totalArr(x)(y)))

            }

        }}

        stepData

    }

}