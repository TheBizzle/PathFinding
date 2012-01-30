package astar_base

import heuristics.HeuristicBundle
import pathfinding.PathFinder
import pathfinding.pathingmap.PathingMap
import pathfinding.coordinate.{PriorityCoordinate, Coordinate}
import pathfinding.statuses._
import pathfinding.breadcrumb.Breadcrumb

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:15 PM
 */

abstract class AStarBase[T <: AStarStepData](branchingFactor: Double, heuristicFunc: HeuristicBundle => Int) extends AStarLike[T] with AStarGruntwork[T] with PathFinder[T] {

    protected val scalingFactor = branchingFactor        // How much of the map you're willing to query (from 0 to 1)
    protected val heuristic = heuristicFunc              // The heuristic function that A* will be using

    override protected def decide(stepData: T, maxIters: Int) : PathingStatus[T] = {

        import stepData._

        if (!queue.isEmpty && (iters < maxIters)) {

            val freshOption = getFreshLoc(queue, beenThereArr)
            if (freshOption == None) return Failure(stepData)         // Exit point (failure)

            val freshLoc = freshOption.get
            pathingMap.step(loc, freshLoc)

            if (goalIsFound(stepData, freshLoc))
                return Success(makeNewStepData(freshLoc, stepData))     // Exit point (success)

            stepData.incIters()
            beenThereArr(freshLoc.x)(freshLoc.y) = true
            Continue(makeNewStepData(freshLoc, stepData))               // Exit point (only to return again soon)

        }
        else
            Failure(makeNewStepData(new Coordinate(), stepData))  // Exit point (failure)

    }

    override protected def step(stepData: T) : (T, List[Breadcrumb]) = {

        import stepData._
        var outList = List[Breadcrumb]()         // Ewwwwww

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

                    val crumb = new Coordinate(loc.x, loc.y)
                    breadcrumbArr(x)(y) = crumb
                    outList = Breadcrumb(neighbor, crumb) :: outList
                    
                }

                if (!doesContainNeighbor)
                    queue.enqueue(new PriorityCoordinate(neighbor, totalArr(x)(y)))

            }

        }}

        (stepData, outList)

    }

    override protected def primeStepData(stepData: T) : T = {
        import stepData._
        costArr(loc.x)(loc.y) = 0
        heuristicArr(loc.x)(loc.y) = heuristic(new HeuristicBundle(loc, goal))
        totalArr(loc.x)(loc.y) = costArr(loc.x)(loc.y) + heuristicArr(loc.x)(loc.y)
        stepData
    }

}