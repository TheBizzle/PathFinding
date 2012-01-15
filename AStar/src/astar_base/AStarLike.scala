package astar_base

import pathfinding.coordinate.Coordinate
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.statuses.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:04 PM
 */

trait AStarLike[T <: AStarStepData] {

    def apply(mapString: PathingMapString) : ExecutionStatus[T]
    protected def execute(stepData: T,  iters: Int = 0, maxIters: Int) : ExecutionStatus[T]
    protected def goalIsFound(inSeq: Any*) : Boolean
    protected def makeNewStepData(freshLoc: Coordinate, stepData: T) : T
    protected def decide(stepData: T, iters: Int, maxIters: Int) : ExecutionStatus[T]
    protected def step(stepData: T) : T
    protected def primeStepData(stepData: T) : T

}