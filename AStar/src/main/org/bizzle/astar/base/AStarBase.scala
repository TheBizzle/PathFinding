package org.bizzle.astar.base

import
  scala.collection.mutable.ListBuffer

import
  org.bizzle.pathfinding.{ coordinate, PathFinder, pathingmap, PathingStatus },
    coordinate.{ Breadcrumb, Coordinate, PriorityCoordinate },
    pathingmap.PathingMap,
    PathingStatus._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:15 PM
 */

abstract class AStarBase[T <: AStarStepData](branchingFactor: Double, heuristicFunc: HeuristicBundle => Int) extends AStarLike[T] with AStarGruntwork[T] with PathFinder[T] {

  protected val scalingFactor = branchingFactor // How much of the map you're willing to query (from 0 to 1)
  protected val heuristic     = heuristicFunc   // The heuristic function that A* will be using

  override protected def decide(stepData: T, maxIters: Int) : PathingStatus[T] = {
    import stepData._
    if (!queue.isEmpty && (iters < maxIters))
      (for (freshLoc <- getFreshLoc(queue, beenThereArr)) yield {
        pathingMap.step(loc, freshLoc)
        if (goalIsFound(stepData, freshLoc))
          Success(makeNewStepData(stepData, freshLoc))
        else {
          beenThereArr(freshLoc.x)(freshLoc.y) = true
          Continue(makeNewStepData(stepData, freshLoc, true))
        }
      }) getOrElse Failure(stepData)
    else
      Failure(makeNewStepData(stepData))
  }

  override protected def step(stepData: T) : (T, Seq[Breadcrumb]) = {

    import stepData._
    val crumbs = new ListBuffer[Breadcrumb]()

    pathingMap.neighborsOf(loc) foreach {
      n =>

        val neighbor = PathingMap.findNeighborCoord(loc, n)
        import neighbor.{x, y}

        if (!beenThereArr(x)(y)) {

          val newCost = costArr(loc.x)(loc.y) + 1
          val doesContainNeighbor = queue exists (x => neighbor overlaps x)

          if (!doesContainNeighbor || (newCost < costArr(x)(y))) {

            costArr(x)(y) = newCost
            heuristicArr(x)(y) = heuristic(HeuristicBundle(neighbor, goal))
            totalArr(x)(y) = costArr(x)(y) + heuristicArr(x)(y)

            val crumb = Coordinate(loc.x, loc.y)
            breadcrumbArr(x)(y) = crumb
            crumbs += Breadcrumb(neighbor, crumb)

          }

          if (!doesContainNeighbor)
            queue.enqueue(PriorityCoordinate(neighbor, totalArr(x)(y)))

        }

    }

    (stepData, crumbs.toSeq)

  }

  override protected def primeStepData(stepData: T) : T = {
    import stepData._
    costArr(loc.x)(loc.y) = 0
    heuristicArr(loc.x)(loc.y) = heuristic(HeuristicBundle(loc, goal))
    totalArr(loc.x)(loc.y) = costArr(loc.x)(loc.y) + heuristicArr(loc.x)(loc.y)
    stepData
  }

}
