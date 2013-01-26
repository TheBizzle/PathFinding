package astar.base

import
  pathfinding.{ coordinate, pathingmap, PathingStatus },
    coordinate.{ BadCoordinate2D, Breadcrumb, Coordinate2D },
    pathingmap.PathingMapString

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:04 PM
 */

trait AStarLike[T <: AStarStepData] {
  /*none */ def apply(mapString: PathingMapString)               : PathingStatus[T]
  protected def decide(stepData: T, maxIters: Int)               : PathingStatus[T]
  protected def execute(stepData: T,  maxIters: Int)             : PathingStatus[T]
  protected def goalIsFound(stepData: T, freshLoc: Coordinate2D) : Boolean
  protected def primeStepData(stepData: T)                       : T
  protected def step(stepData: T)                                : (T, Seq[Breadcrumb])
  protected def makeNewStepData(stepData: T, freshLoc: Coordinate2D = BadCoordinate2D, isIncingIters: Boolean = false) : T
}
