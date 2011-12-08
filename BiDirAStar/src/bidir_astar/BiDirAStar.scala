package bidir_astar

import pathingmap.pathingmapdata.PathingMapString
import astar_base._
import heuristic.HeuristicLib

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 10:40 PM
 */

// Basically, runs two AStar processes asychronously, and they pass each other their updated beenThere arrays and current locations.
// If one reaches a location that the other has reached, or if the current locations are next to each other, it returns.

object BiDirAStar extends AStarBase[BiDirStepData](0.8, HeuristicLib.manhattanDistance) {

    override def apply(mapString: PathingMapString) : BiDirStepData = {

        // Initialize data
        // Manage actors

        null

    }

    private def leadActorsToSuccess() : BiDirStepData = {

        // Create two actors
        // Iterate on each until end

        null

    }

    override protected def iterate(stepData: BiDirStepData, iters: Int, maxIters: Int) : BiDirStepData = {

        null

    }

    override protected def step(stepData: BiDirStepData) : BiDirStepData = {
        null
    }

}