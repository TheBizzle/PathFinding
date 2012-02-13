package astar_base

import pathfinding.coordinate.Coordinate
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.statuses.PathingStatus
import pathfinding.breadcrumb.Breadcrumb

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:04 PM
 */

trait AStarLike[T <: AStarStepData] {

    def apply(mapString: PathingMapString) : PathingStatus[T]
    protected def execute(stepData: T,  maxIters: Int) : PathingStatus[T]
    protected def goalIsFound(stepData: T, freshLoc: Coordinate) : Boolean
    protected def makeNewStepData(stepData: T, freshLoc: Coordinate) : T
    protected def decide(stepData: T, maxIters: Int) : PathingStatus[T]
    protected def step(stepData: T) : (T, List[Breadcrumb])
    protected def primeStepData(stepData: T) : T

}
