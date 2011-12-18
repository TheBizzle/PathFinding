package bidir_astar

import astar_base.StepData
import coordinate.{PriorityCoordinate, Coordinate}
import datastructure.priorityqueue.PriorityQueue
import pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 10:41 PM
 */

// Ugly!
class BiDirStepData(currentLocation: Coordinate,
                    goalLocation: Coordinate,
                    beenThere: Array[Array[Boolean]],
                    pQueue: PriorityQueue[PriorityCoordinate],
                    pMap: PathingMap,
                    costArray: Array[Array[Int]],
                    heuristicArray: Array[Array[Int]],
                    totalArray: Array[Array[Int]],
                    breadcrumbs: Array[Array[Coordinate]],
                    othersBreadcrumbs: Array[Array[Coordinate]])
                    extends StepData(currentLocation, goalLocation, beenThere, pQueue, pMap, costArray, heuristicArray, totalArray, breadcrumbs) {

    // Shame on me!  Shaaaaame!
    private var othersBreadcrumbArr = othersBreadcrumbs

    override def clone() : BiDirStepData = {
        new BiDirStepData(loc.clone(), goal.clone(), beenThereArr map (_.clone()), queue.clone(),
                          pathingMap.clone(), costArr map (_.clone()), heuristicArr map (_.clone()),
                          totalArr map (_.clone()), breadcrumbArr map (_.clone()), othersBreadcrumbArr map (_.clone()))
    }

    def cloneForBiBackwards() : BiDirStepData = {
        new BiDirStepData(goal.clone(), loc.clone(), beenThereArr map (_.clone()), queue.clone(),
                          pathingMap.clone(), costArr map (_.clone()), heuristicArr map (_.clone()),
                          totalArr map (_.clone()), breadcrumbArr map (_.clone()), othersBreadcrumbArr map (_.clone()))
    }

    def mergeShared(oBreadcrumbs: Array[Array[Coordinate]]) {
        othersBreadcrumbArr = oBreadcrumbs.clone()
    }

    def hasInOthersBreadcrumbs(loc: Coordinate) : Boolean = {
        val thatLoc = othersBreadcrumbArr(loc.x)(loc.y)
        (thatLoc.x != Coordinate.InvalidVal) && (thatLoc.y != Coordinate.InvalidVal)
    }

}


object BiDirStepData {
    def apply(freshLoc: Coordinate, stepData: BiDirStepData) : BiDirStepData = {
        import stepData._
        new BiDirStepData(freshLoc, goal, beenThereArr, queue, pathingMap,
                          costArr, heuristicArr, totalArr, breadcrumbArr, othersBreadcrumbArr)
    }
}
