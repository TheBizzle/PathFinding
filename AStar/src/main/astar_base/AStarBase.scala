package astar_base

import heuristics.HeuristicBundle
import pathfinding.PathFinder
import pathfinding.pathingmap.PathingMap
import pathfinding.coordinate.{PriorityCoordinate, Coordinate}
import pathfinding.statuses._
import pathfinding.breadcrumb.Breadcrumb
import collection.mutable.ListBuffer

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

      val freshLoc = getFreshLoc(queue, beenThereArr).getOrElse(return Failure(stepData))   // Exit point (failure)
      pathingMap.step(loc, freshLoc)

      if (goalIsFound(stepData, freshLoc))
        return Success(makeNewStepData(stepData, freshLoc))                               // Exit point (success)

      stepData.incIters()
      beenThereArr(freshLoc.x)(freshLoc.y) = true
      Continue(makeNewStepData(stepData, freshLoc))                                         // Exit point (only to return again soon)

    }
    else
      Failure(makeNewStepData(stepData, Coordinate()))                                      // Exit point (failure)

  }

  override protected def step(stepData: T) : (T, List[Breadcrumb]) = {

    import stepData._
    val outList = new ListBuffer[Breadcrumb]()

    pathingMap.neighborsOf(loc) foreach {
      n =>

        val neighbor = PathingMap.findNeighborCoord(loc, n)
        val x = neighbor.x
        val y = neighbor.y

        if (!beenThereArr(x)(y)) {

          val newCost = costArr(loc.x)(loc.y) + 1
          val doesContainNeighbor = queueDoesContain(neighbor, queue)

          if (!doesContainNeighbor || (newCost < costArr(x)(y))) {

            costArr(x)(y) = newCost
            heuristicArr(x)(y) = heuristic(HeuristicBundle(neighbor, goal))
            totalArr(x)(y) = costArr(x)(y) + heuristicArr(x)(y)

            val crumb = Coordinate(loc.x, loc.y)
            breadcrumbArr(x)(y) = crumb
            outList += Breadcrumb(neighbor, crumb)

          }

          if (!doesContainNeighbor)
            queue.enqueue(new PriorityCoordinate(neighbor, totalArr(x)(y)))

        }

    }

    (stepData, outList.toList)

  }

  override protected def primeStepData(stepData: T) : T = {
    import stepData._
    costArr(loc.x)(loc.y) = 0
    heuristicArr(loc.x)(loc.y) = heuristic(HeuristicBundle(loc, goal))
    totalArr(loc.x)(loc.y) = costArr(loc.x)(loc.y) + heuristicArr(loc.x)(loc.y)
    stepData
  }

}
