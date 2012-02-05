package astar_base.bidir_astar

import concurrency.BiDirDirector
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import astar_base._
import exceptions.UnexpectedDataException
import heuristics.HeuristicLib
import pathfinding.coordinate._
import pathfinding.statuses._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 10:40 PM
 */

// Basically, runs two AStar processes asychronously, and they pass each other their updated beenThere arrays and current locations.
// If one reaches a location that the other has reached, or if the current locations are next to each other, it returns.
object BiDirAStar extends AStarBase[BiDirStepData](0.8, HeuristicLib.manhattanDistance) {

    override def apply(mapString: PathingMapString) : PathingStatus[BiDirStepData] = {
        val stepData = BiDirStepData(mapString)
        execute(primeStepData(stepData), maxIters = calculateMaxIters(stepData.pathingMap.colCount, stepData.pathingMap.rowCount))
    }

    override protected def execute(stepData: BiDirStepData, maxIters: Int) : PathingStatus[BiDirStepData] = {

        val (stgStepData, stgCrumbs) = step(stepData.clone())
        val (gtsStepData, gtsCrumbs) = step(stepData.cloneForBiBackwards())
        stgStepData.assimilateBreadcrumbs(gtsCrumbs)
        gtsStepData.assimilateBreadcrumbs(stgCrumbs)
        val director = new BiDirDirector(decide(_: BiDirStepData, maxIters), step)    // decide() gets partially applied

        director.direct(stgStepData, gtsStepData) match {
            case status: PathingStatus[BiDirStepData] => status
            case _ => throw new UnexpectedDataException
        }
        
    }

    override protected def goalIsFound(inSeq: Any*) : Boolean = {
        val stepData = inSeq(0).asInstanceOf[BiDirStepData]
        val freshLoc = inSeq(1).asInstanceOf[Coordinate]
        (freshLoc overlaps stepData.goal) || (stepData hasInOthersBreadcrumbs freshLoc)
    }

    override protected def makeNewStepData(freshLoc: Coordinate, stepData: BiDirStepData) : BiDirStepData = {
        BiDirStepData(freshLoc, stepData)
    }

}
