package astar_base

import heuristic.HeuristicBundle
import pathingmap.pathingmapdata.PathingMapString
import statuses.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 11:15 PM
 */

abstract class AStarBase[T <: StepData](branchingFactor: Double, heuristicFunc: HeuristicBundle => Int) extends AStarLike[T] {

    // A bad/initial numerical value for some things—NOT COUNTING THE "X" AND "Y" MEMBERS OF COORDINATE OBJECTS; they have their own InvalidValue
    protected val BadVal = -1

    protected val scalingFactor = branchingFactor        // How much of the map you're willing to query (from 0 to 1)
    protected val heuristic = heuristicFunc              // The heuristic function that A* will be using

    def apply(mapString: PathingMapString) : ExecutionStatus[T]
    protected def execute(stepData: T,  iters: Int = 0, maxIters: Int) : ExecutionStatus[T]
    protected def decide(stepData: T, iters: Int, maxIters: Int) : ExecutionStatus[T]
    protected def step(stepData: T) : T

}