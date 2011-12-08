package astar_base

import heuristic.HeuristicBundle
import pathingmap.pathingmapdata.PathingMapString

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:15 PM
 */

abstract class AStarBase[T >: StepData](branchingFactor: Double, heuristicFunc: HeuristicBundle => Int) extends AStarLike[T] {

    protected val scalingFactor = branchingFactor        // How much of the map you're willing to query (from 0 to 1)
    protected val heuristic = heuristicFunc            // The heuristic function that A* will be using

    def apply(mapString: PathingMapString) : T
    protected def iterate(stepData: T, iters: Int, maxIters: Int) : T
    protected def step(stepData: T) : T

}