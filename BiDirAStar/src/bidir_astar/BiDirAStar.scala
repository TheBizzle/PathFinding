package bidir_astar

import pathingmap.pathingmapdata.PathingMapString
import astar_base._
import coordinate.Coordinate
import heuristic.HeuristicLib

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 10:40 PM
 */

// Basically, runs two AStar processes asychronously, and they pass each other their updated beenThere arrays and current locations.
// If one reaches a location that the other has reached, or if the current locations are next to each other, it returns.

object BiDirAStar extends AStarBase(0.8, HeuristicLib.manhattanDistance) with AStarLike {

    override def apply(mapString: PathingMapString) : StepData = {

        null

    }

    override protected def iterate(stepData: StepData, iters: Int, maxIters: Int) : StepData = {

        null

    }

    override protected def step(stepData: StepData) : StepData = {
        null
    }

}