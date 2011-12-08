package astar_base

import heuristic.HeuristicBundle
import pathingmap.pathingmapdata.PathingMapString

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:15 PM
 */

abstract class AStarBase(scalingFactor: Double, heuristicFunc: HeuristicBundle => Int) {

    protected val ScalingFactor = scalingFactor        // How much of the map you're willing to query (from 0 to 1)
    protected val heuristic = heuristicFunc            // The heuristic function that A* will be using

    def apply(mapString: PathingMapString) : StepData
    protected def iterate(stepData: StepData, iters: Int, maxIters: Int) : StepData
    protected def step(stepData: StepData) : StepData

}