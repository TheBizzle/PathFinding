package astar

import
  pathfinding.{ coordinate, pathingmap, PathingStatus },
    coordinate.{ BadCoordinate2D, Coordinate2D },
    pathingmap.PathingMapString

import
  base.{ AStarBase, HeuristicLib }

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
    execute(primeStepData(stepData), calculateMaxIters(stepData.pathingMap.colCount, stepData.pathingMap.rowCount))
  }

  override protected def execute(stepData: BiDirStepData, maxIters: Int) : PathingStatus[BiDirStepData] = {

    val (stgStepData, stgCrumbs) = step(stepData.clone())
    val (gtsStepData, gtsCrumbs) = step(stepData.cloneForBiBackwards())
    stgStepData.assimilateBreadcrumbs(gtsCrumbs)
    gtsStepData.assimilateBreadcrumbs(stgCrumbs)

    val director = new BiDirDirector(decide(_: BiDirStepData, maxIters), step)    // decide() gets partially applied
    director.direct(stgStepData, gtsStepData)

  }

  override protected def goalIsFound(stepData: BiDirStepData, freshLoc: Coordinate2D) =
    (freshLoc overlaps stepData.goal) || (stepData hasInOthersBreadcrumbs freshLoc)

  override protected def makeNewStepData(stepData: BiDirStepData, freshLoc: Coordinate2D = BadCoordinate2D, isIncingIters: Boolean = false) =
    BiDirStepData(freshLoc, stepData, isIncingIters)

}
