package astar_base

import pathfinding.coordinate.{ BadCoordinate2D, Breadcrumb, Coordinate2D }
import pathfinding.pathingmap.PathingMapString
import pathfinding.PathingStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:04 PM
 */

trait AStarLike[T <: AStarStepData] {
            def apply(mapString: PathingMapString)               : PathingStatus[T]
  protected def decide(stepData: T, maxIters: Int)               : PathingStatus[T]
  protected def execute(stepData: T,  maxIters: Int)             : PathingStatus[T]
  protected def goalIsFound(stepData: T, freshLoc: Coordinate2D) : Boolean
  protected def primeStepData(stepData: T)                       : T
  protected def step(stepData: T)                                : (T, Seq[Breadcrumb])
  protected def makeNewStepData(stepData: T, freshLoc: Coordinate2D = BadCoordinate2D, isIncingIters: Boolean = false) : T
}
