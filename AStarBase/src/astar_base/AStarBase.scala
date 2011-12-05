package astar_base

import pathingmap.pathingmapdata.PathingMapString
import coordinate.Coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:15 PM
 */

abstract class AStarBase(scalingFactor: Double) {

    protected val ScalingFactor = scalingFactor        // How much of the map you're willing to query (from 0 to 1)

    def apply(mapString: PathingMapString) : StepData
    protected def iterate(stepData: StepData, iters: Int, maxIters: Int) : StepData
    protected def generateNewStepData(stepData: StepData, freshLoc: Coordinate) : StepData
    protected def step(stepData: StepData) : StepData

}