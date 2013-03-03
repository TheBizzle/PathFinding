package org.bizzle.astar.base

import
  scala.collection.mutable.PriorityQueue

import
  org.bizzle.pathfinding.{ coordinate, pathingmap, StepData, StepDataSingleton },
    coordinate.{ BadCoordinate2D, Coordinate2D, PriorityCoordinate },
    pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 10:46 PM
 */
class AStarStepData(currentLocationOpt: Coordinate2D,
                    goalLocation: Coordinate2D,
                    val beenThereArr: Array[Array[Boolean]],
                    val queue: PriorityQueue[PFCoord],
                    pMap: PathingMap,
                    val costArr: Array[Array[Int]],
                    val heuristicArr: Array[Array[Int]],
                    val totalArr: Array[Array[Int]],
                    breadcrumbs: Array[Array[Coordinate2D]],
                    val iters: Int = 0,
                    endGoalLocation: Coordinate2D = BadCoordinate2D) extends StepData(currentLocationOpt, goalLocation, pMap, breadcrumbs, endGoalLocation)

object AStarStepData extends StepDataSingleton[AStarStepData] with StepDataGenerator[AStarStepData] {

  import shapeless._

  override type Extras = HNil

  override def apply(freshLoc: Coordinate2D, stepData: AStarStepData, isIncingIters: Boolean = false) : AStarStepData = {
    import stepData._
    val iterations = if (isIncingIters) iters + 1 else iters
    new AStarStepData(freshLoc, goal, beenThereArr, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr, iterations, endGoal)
  }

  override protected def generateExtras(stepData: AStarStepData)                 : Extras        = HNil
  override protected def mixinExtras   (stepData: AStarStepData, extras: Extras) : AStarStepData = stepData

}
