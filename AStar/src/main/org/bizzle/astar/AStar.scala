package org.bizzle.astar

import
  scala.annotation.tailrec

import
  org.bizzle.pathfinding.{ coordinate, pathingmap, PathingStatus },
    coordinate.{ BadCoordinate2D, Coordinate2D },
    pathingmap.PathingMapString,
    PathingStatus._

import
  base.{ AStarBase, AStarStepData, HeuristicLib }

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

  override def apply(mapString: PathingMapString) : PathingStatus[AStarStepData] = {
    val stepData = AStarStepData(mapString)
    execute(primeStepData(stepData), calculateMaxIters(stepData.pathingMap.colCount, stepData.pathingMap.rowCount))
  }

  override protected def execute(stepData: AStarStepData, maxIters: Int) : PathingStatus[AStarStepData] = {
    @tailrec def executeHelper(stepData: AStarStepData, maxIters: Int) : PathingStatus[AStarStepData] = {
      val decision = decide(stepData, maxIters)
      decision match {
        case Continue(x: AStarStepData) => executeHelper(step(x)._1, maxIters)
        case Success(_)                 => decision
        case Failure(_)                 => decision
      }
    }
    executeHelper(step(stepData)._1, maxIters)
  }

  protected def goalIsFound    (stepData: AStarStepData, freshLoc: Coordinate2D)                                                   = freshLoc overlaps stepData.goal
  protected def makeNewStepData(stepData: AStarStepData, freshLoc: Coordinate2D = BadCoordinate2D, isIncingIters: Boolean = false) = AStarStepData(freshLoc, stepData, isIncingIters)

}
