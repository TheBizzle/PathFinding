package bidir_astar

import astar_base.StepData
import coordinate.{PriorityCoordinate, Coordinate}
import datastructure.priorityqueue.PriorityQueue
import pathingmap.PathingMap
import scala.Array

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
                    othersBreadcrumbs: Array[Array[Coordinate]],
                    endGoalLocation: Coordinate = null)
                    extends StepData(currentLocation, goalLocation, beenThere, pQueue, pMap, costArray, heuristicArray, totalArray, breadcrumbs, endGoalLocation) {

    // Shame on me!  Shaaaaame!
    private var othersBreadcrumbArr = othersBreadcrumbs

    override def clone() : BiDirStepData = {
        cloneBase()
    }

    def cloneForBiBackwards() : BiDirStepData = {
        cloneBase(l = goal, g = loc, tg = goal)
    }

    // HIDEOUS!  ...But useful.
    private def cloneBase(l: Coordinate = loc, g: Coordinate = goal, bThere: Array[Array[Boolean]] = beenThereArr,
                          q: PriorityQueue[PriorityCoordinate] = queue, pm: PathingMap = pathingMap,
                          cost: Array[Array[Int]] = costArr, h: Array[Array[Int]] = heuristicArr,
                          total: Array[Array[Int]] = totalArr, crumbs: Array[Array[Coordinate]] = breadcrumbArr,
                          otherCrumbs: Array[Array[Coordinate]] = othersBreadcrumbArr, tg: Coordinate = endGoal) : BiDirStepData = {
        new BiDirStepData(l.clone(), g.clone(), bThere map (_.clone()), q.clone(), pm.clone(),
                          cost map (_.clone()), h map (_.clone()), total map (_.clone()),
                          crumbs map (_.clone()), otherCrumbs map (_.clone()), tg.clone())
    }

    def assimilateBreadcrumbs(oBreadcrumbs: Array[Array[Coordinate]]) {
        othersBreadcrumbArr = oBreadcrumbs.clone()
    }

    def transformGoalForClone(oGoal: Coordinate) : BiDirStepData = {
        cloneBase(g = oGoal)
    }

    def hasInOthersBreadcrumbs(loc: Coordinate) : Boolean = {
        val thatLoc = othersBreadcrumbArr(loc.x)(loc.y)
        (thatLoc.x != Coordinate.InvalidValue) && (thatLoc.y != Coordinate.InvalidValue)
    }

    def reverseBreadcrumbs() {
        def reversalHelper(loc: Coordinate, crumbArr: Array[Array[Coordinate]]) {
            val crumb = crumbArr(loc.x)(loc.y)
            if (crumb.x != Coordinate.InvalidValue && crumb.y != Coordinate.InvalidValue) {
                reversalHelper(crumb, crumbArr)
                crumbArr(crumb.x)(crumb.y) = loc
            }
        }
        // DEBUGGING STATEMENTS
        //breadcrumbArr foreach ( x => print( x(0).toString + "||") ); print('\n')
        reversalHelper(loc, breadcrumbArr)
        breadcrumbArr(loc.x)(loc.y) = new Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue)
        //breadcrumbArr foreach ( x => print( x(0).toString + "||") ); print('\n')
    }

}


object BiDirStepData {
    def apply(freshLoc: Coordinate, stepData: BiDirStepData) : BiDirStepData = {
        import stepData._
        new BiDirStepData(freshLoc, goal, beenThereArr, queue, pathingMap, costArr,
                          heuristicArr, totalArr, breadcrumbArr, othersBreadcrumbArr, endGoal)
    }
}
