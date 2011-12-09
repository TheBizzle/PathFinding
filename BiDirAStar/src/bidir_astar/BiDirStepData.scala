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
                    othersCurrentLocation: Coordinate,
                    othersBreadcrumbs: Array[Array[Coordinate]])
                    extends StepData(currentLocation, goalLocation, beenThere, pQueue, pMap, costArray, heuristicArray, totalArray, breadcrumbs) {

    val othersLoc = othersCurrentLocation
    val othersBreadcrumbArr = othersBreadcrumbs

    override def clone() : BiDirStepData = {
        new BiDirStepData(loc.clone(), goal.clone(), beenThereArr.clone(), queue.clone(),
                          pathingMap.clone(), costArr.clone(), heuristicArr.clone(), totalArr.clone(),
                          breadcrumbArr.clone(), othersLoc.clone(), othersBreadcrumbArr.clone())
    }

    def cloneForBiBackwards() : BiDirStepData = {
        val neoStart = loc.clone()
        new BiDirStepData(goal.clone(), neoStart, beenThereArr.clone(), queue.clone(),
                          pathingMap.clone(), costArr.clone(), heuristicArr.clone(), totalArr.clone(),
                          breadcrumbArr.clone(), neoStart, othersBreadcrumbArr.clone())
    }

}


object BiDirStepData {

    def apply(freshLoc: Coordinate, stepData: BiDirStepData) : BiDirStepData = {
        import stepData._
        new BiDirStepData(freshLoc, goal, beenThereArr, queue, pathingMap,
        costArr, heuristicArr, totalArr, breadcrumbArr,
        othersLoc, othersBreadcrumbArr)
    }

    def importShared(stepData: BiDirStepData, oLoc: Coordinate, oBreadcrumbs: Array[Array[Coordinate]]) : BiDirStepData = {
        import stepData._
        new BiDirStepData(loc, goal, beenThereArr, queue, pathingMap,
                          costArr, heuristicArr, totalArr, breadcrumbArr, oLoc.clone(), oBreadcrumbs.clone())
    }

}