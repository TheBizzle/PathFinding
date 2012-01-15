package astar_base.bidir_astar

import pathfinding.coordinate._
import datastructure.priorityqueue.PriorityQueue
import pathfinding.pathingmap.PathingMap
import pathfinding.StepDataSingleton
import astar_base.{FactoryThatTakesAStarStepData, AStarStepData}

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
                    extends AStarStepData(currentLocation, goalLocation, beenThere, pQueue, pMap, costArray, heuristicArray, totalArray, breadcrumbs, endGoalLocation) {

    // Shame on me!  Shaaaaame!
    private var othersBreadcrumbArr = othersBreadcrumbs

    override def clone() : BiDirStepData = {
        cloneBase()
    }

    def cloneForBiBackwards() : BiDirStepData = {
        cloneBase(l = goal, g = loc, eg = goal)
    }

    // HIDEOUS!  ...But useful.
    private def cloneBase(l: Coordinate = loc, g: Coordinate = goal, bThere: Array[Array[Boolean]] = beenThereArr,
                          q: PriorityQueue[PriorityCoordinate] = queue, pm: PathingMap = pathingMap,
                          cost: Array[Array[Int]] = costArr, h: Array[Array[Int]] = heuristicArr,
                          total: Array[Array[Int]] = totalArr, crumbs: Array[Array[Coordinate]] = breadcrumbArr,
                          otherCrumbs: Array[Array[Coordinate]] = othersBreadcrumbArr, eg: Coordinate = endGoal) : BiDirStepData = {
        new BiDirStepData(l.clone(), g.clone(), bThere map (_.clone()), q.clone(), pm.clone(),
                          cost map (_.clone()), h map (_.clone()), total map (_.clone()),
                          crumbs map (_.clone()), otherCrumbs map (_.clone()), eg.clone())
    }

    def assimilateBreadcrumbs(oBreadcrumbs: Array[Array[Coordinate]]) {
        othersBreadcrumbArr = oBreadcrumbs.clone()
    }

    def transformGoalForClone(oGoal: Coordinate) : BiDirStepData = {
        cloneBase(g = oGoal)
    }

    def hasInOthersBreadcrumbs(loc: Coordinate) : Boolean = {
        othersBreadcrumbArr(loc.x)(loc.y) match {
            case Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue) => false
            case _                                                            => true
        }
    }

    def reverseBreadcrumbs() {
        def reversalHelper(loc: Coordinate, crumbArr: Array[Array[Coordinate]]) {
            val crumb = crumbArr(loc.x)(loc.y)
            crumb match {
                case Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue) => // Let the recursion die on invalid Coordinates
                case _ => {
                    reversalHelper(crumb, crumbArr)
                    crumbArr(crumb.x)(crumb.y) = loc
                }
            }
        }
        reversalHelper(loc, breadcrumbArr)
        breadcrumbArr(loc.x)(loc.y) = new Coordinate()
    }

}


object BiDirStepData extends StepDataSingleton[BiDirStepData] with FactoryThatTakesAStarStepData[BiDirStepData] {
    
    def apply(freshLoc: Coordinate, stepData: BiDirStepData) : BiDirStepData = {
        import stepData._
        new BiDirStepData(freshLoc, goal, beenThereArr, queue, pathingMap, costArr,
                          heuristicArr, totalArr, breadcrumbArr, othersBreadcrumbArr, endGoal)
    }

    override protected def generateExtras(stepData: AStarStepData) : Seq[Any] = {
        import stepData.pathingMap._
        val otherBreadcrumbs = initialize2DArr(colCount, rowCount, new Coordinate())
        Seq(otherBreadcrumbs)
    }

    override protected def mixinExtras(stepData: AStarStepData, extras: Seq[Any]) : BiDirStepData = {
        import stepData._
        val otherBreadcrumbs = extras(0).asInstanceOf[Array[Array[Coordinate]]]
        new BiDirStepData(loc, goal, beenThereArr, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr, otherBreadcrumbs)
    }

}
