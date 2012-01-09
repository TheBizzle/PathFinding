package astar_base.astar

//import collection.mutable.PriorityQueue
import datastructure.priorityqueue.PriorityQueue
import astar_base._
import heuristics.{HeuristicBundle, HeuristicLib}
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.statuses._
import pathfinding.pathingmap.PathingMap
import pathfinding.coordinate._
import annotation.tailrec

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

object AStar extends AStarBase[AStarStepData](1.0, HeuristicLib.manhattanDistance) {

    override def apply(mapString: PathingMapString) : ExecutionStatus[AStarStepData] = {

        val (start, goal, pathingMap) = PathingMap(mapString)

        val colCount = pathingMap.colCount
        val rowCount = pathingMap.rowCount

        val beenThere = initialize2DArr(colCount, rowCount, false)
        val queue = new PriorityQueue[PriorityCoordinate](PriorityCoordinateOrdering.compare)

        val costArr = initialize2DArr(colCount, rowCount, BadVal)
        val heuristicArr = initialize2DArr(colCount, rowCount, BadVal)
        val totalArr = initialize2DArr(colCount, rowCount, BadVal)
        val breadcrumbArr = initialize2DArr(colCount, rowCount, new Coordinate())

        costArr(start.x)(start.y) = 0
        heuristicArr(start.x)(start.y) = heuristic(new HeuristicBundle(start, goal))
        totalArr(start.x)(start.y) = costArr(start.x)(start.y) + heuristicArr(start.x)(start.y)

        queue.enqueue(new PriorityCoordinate(start, totalArr(start.x)(start.y)))
        execute(new AStarStepData(start, goal, beenThere, queue, pathingMap,
                             costArr, heuristicArr, totalArr, breadcrumbArr),
                maxIters = calculateMaxIters(colCount, rowCount))

    }

    @tailrec
    override protected def execute(stepData: AStarStepData, iters: Int = 0, maxIters: Int) : ExecutionStatus[AStarStepData] = {

        val decision = decide(stepData, iters, maxIters)

        decision match {
            case Continue(x: AStarStepData) => execute(step(x), iters + 1, maxIters)
            case Success(_)  => decision
            case Failure(_)  => decision
        }

    }

    override protected def decide(stepData: AStarStepData, iters: Int, maxIters: Int) : ExecutionStatus[AStarStepData] = {

        import stepData._

        if (!queue.isEmpty && (iters < maxIters)) {

            val freshOption = getFreshLoc(queue, beenThereArr)
            if (freshOption == None) return Failure(stepData)      // Exit point (failure)

            val freshLoc = freshOption.get
            pathingMap.step(loc, freshLoc)

            // DEBUGGING STATEMENTS
            //println(pathingMap.toString + "\n")
            //println("Loc: " + loc)
            //println("Goal: " + destination)
            //println(stepData.queue.toString() + "\n\n")

            if (freshLoc overlaps goal)
                return Success(AStarStepData(freshLoc, stepData))  // Exit point (success)

            beenThereArr(freshLoc.x)(freshLoc.y) = true
            Continue(AStarStepData(freshLoc, stepData))

        }
        else
            Failure(AStarStepData(new Coordinate(), stepData))  // Exit point (failure)

    }

    override protected def step(stepData: AStarStepData) : AStarStepData = {

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