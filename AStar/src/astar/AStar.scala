package astar

//import collection.mutable.PriorityQueue
import pathingmap.pathingmapdata.PathingMapString
import pathingmap.PathingMap
import datastructure.priorityqueue.PriorityQueue
import coordinate._
import astar_base._
import heuristic.{HeuristicBundle, HeuristicLib}
import statuses._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 3:33 PM
 */

// As A* traverses the graph, it follows a path of the lowest known cost, keeping a sorted priority queue of alternate path segments along the way.
// If, at any point, a segment of the path being traversed has a higher cost than another encountered path segment, it abandons the higher-cost path
// segment and traverses the lower-cost path segment instead. This process continues until the destination is reached.

// Already-spent cost   (from origin to current)        g(x)
// Heuristic estimate   (from current to destination)   h(x)
// Total combined cost  (from origin to destination)    f(x) = g(x) + h(x)

object AStar extends AStarBase[StepData](1.0, HeuristicLib.manhattanDistance) {

    override def apply(mapString: PathingMapString) : ExecutionStatus[StepData] = {

        val (start, goal, pathingMap) = PathingMap(mapString)

        val colCount = pathingMap.colCount
        val rowCount = pathingMap.rowCount

        val beenThere = initialize2DArr(colCount, rowCount, false)
        val queue = new PriorityQueue[PriorityCoordinate](PriorityCoordinateOrdering.compare)

        val costArr = initialize2DArr(colCount, rowCount, BadVal)
        val heuristicArr = initialize2DArr(colCount, rowCount, BadVal)
        val totalArr = initialize2DArr(colCount, rowCount, BadVal)
        val breadcrumbArr = initialize2DArr(colCount, rowCount, new Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue))

        costArr(start.x)(start.y) = 0
        heuristicArr(start.x)(start.y) = heuristic(new HeuristicBundle(start, goal))
        totalArr(start.x)(start.y) = costArr(start.x)(start.y) + heuristicArr(start.x)(start.y)

        queue.enqueue(new PriorityCoordinate(start, totalArr(start.x)(start.y)))
        execute(new StepData(start, goal, beenThere, queue, pathingMap,
                             costArr, heuristicArr, totalArr, breadcrumbArr),
                maxIters = calculateMaxIters(colCount, rowCount))

    }

    override protected def execute(stepData: StepData, iters: Int = 0, maxIters: Int) : ExecutionStatus[StepData] = {

        val decision = decide(stepData, iters, maxIters)

        decision match {
            case Continue(x: StepData) => execute(step(x), iters + 1, maxIters)
            case Success(_)  => decision
            case Failure(_)  => decision
        }

    }

    override protected def decide(stepData: StepData, iters: Int, maxIters: Int) : ExecutionStatus[StepData] = {

        import stepData._

        if (!queue.isEmpty && (iters < maxIters)) {

            val freshLoc = getFreshLoc(queue, beenThereArr)
            pathingMap.step(loc, freshLoc)

            // DEBUGGING STATEMENTS
            //println(pathingMap.toString + "\n")
            //println("Loc: " + loc)
            //println("Goal: " + destination)
            //println(stepData.queue.toString() + "\n\n")

            if (freshLoc overlaps goal)
                return Success(StepData(freshLoc, stepData))  // Exit point (success)

            beenThereArr(freshLoc.x)(freshLoc.y) = true
            Continue(StepData(freshLoc, stepData))

        }
        else
            Failure(StepData(new Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue), stepData))  // Exit point (failure)

    }

    override protected def step(stepData: StepData) : StepData = {

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