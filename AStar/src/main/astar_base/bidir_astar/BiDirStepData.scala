package astar_base.bidir_astar

import collection.mutable.PriorityQueue

import pathfinding.{ breadcrumb, coordinate, pathingmap, StepDataSingleton }
import breadcrumb.Breadcrumb
import coordinate.{ BadCoordinate2D, Coordinate2D, PriorityCoordinate }
import pathingmap.PathingMap

import astar_base.{ AStarStepData, exceptions, FactoryThatTakesAStarStepData }, exceptions.UnexpectedDataException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/4/11
 * Time: 10:41 PM
 */

class BiDirStepData(currentLocation: Coordinate2D,
                    goalLocation: Coordinate2D,
                    beenThere: Array[Array[Boolean]],
                    pQueue: PriorityQueue[Coordinate2D with PriorityCoordinate],
                    pMap: PathingMap,
                    costArray: Array[Array[Int]],
                    heuristicArray: Array[Array[Int]],
                    totalArray: Array[Array[Int]],
                    breadcrumbs: Array[Array[Coordinate2D]],
                    private val othersBreadcrumbArr: Array[Array[Coordinate2D]],
                    iterationCount: Int,
                    endGoalLocation: Coordinate2D = BadCoordinate2D)
                    extends AStarStepData(currentLocation, goalLocation, beenThere, pQueue, pMap, costArray, heuristicArray, totalArray, breadcrumbs, iterationCount, endGoalLocation) {

  override def clone()               = cloneBase()
  /*none*/ def cloneForBiBackwards() = cloneBase(l = goal, g = loc, eg = goal)

  // HIDEOUS!  ...But useful.
  private def cloneBase(l: Coordinate2D = loc, g: Coordinate2D = goal, bThere: Array[Array[Boolean]] = beenThereArr,
                        q: PriorityQueue[Coordinate2D with PriorityCoordinate] = queue, pm: PathingMap = pathingMap,
                        cost: Array[Array[Int]] = costArr, h: Array[Array[Int]] = heuristicArr,
                        total: Array[Array[Int]] = totalArr, crumbs: Array[Array[Coordinate2D]] = breadcrumbArr,
                        otherCrumbs: Array[Array[Coordinate2D]] = othersBreadcrumbArr, itrs: Int = iters, eg: Coordinate2D = endGoal) : BiDirStepData = {
    new BiDirStepData(l.copy, g.copy, bThere map (_.clone()), q.clone(), pm.clone(),
                      cost map (_.clone()), h map (_.clone()), total map (_.clone()),
                      crumbs map (_.clone()), otherCrumbs map (_.clone()), itrs, eg.copy)
                      //@ Concoct something for infinite deep-cloning
  }

  final def assimilateBreadcrumbs(crumbs: Seq[Breadcrumb]) {
    crumbs foreach (c => othersBreadcrumbArr(c.to.x)(c.to.y) = c.from)
  }

  def transformGoalForClone(oGoal: Coordinate2D) : BiDirStepData = cloneBase(g = oGoal)
  def hasInOthersBreadcrumbs(loc: Coordinate2D)                  = othersBreadcrumbArr(loc.x)(loc.y).isValid

  def reverseBreadcrumbs() {
    def reversalHelper(location: Coordinate2D, crumbArr: Array[Array[Coordinate2D]]) {
      val crumb = crumbArr(location.x)(location.y)
      if (crumb.isValid) {
        reversalHelper(crumb, crumbArr) //@ Reduce all this casting with some implicits
        crumbArr(crumb.x)(crumb.y) = location
      }
    }
    reversalHelper(loc, breadcrumbArr)
    breadcrumbArr(loc.x)(loc.y) = BadCoordinate2D
  }

}


object BiDirStepData extends StepDataSingleton[BiDirStepData] with FactoryThatTakesAStarStepData[BiDirStepData] {

  import shapeless._

  override type Extras = Array[Array[Coordinate2D]] :: HNil //@ Give the head type here a friendly alias

  def apply(freshLoc: Coordinate2D, stepData: BiDirStepData, isIncingIters: Boolean = false) : BiDirStepData = {
    import stepData._
    new BiDirStepData(freshLoc, goal, beenThereArr, queue, pathingMap, costArr, heuristicArr,
                      totalArr, breadcrumbArr, othersBreadcrumbArr, if (isIncingIters) iters + 1 else iters, endGoal)
  }

  override protected def generateExtras(stepData: AStarStepData) : Extras = {
    import stepData.pathingMap._
    val otherBreadcrumbs: Array[Array[Coordinate2D]]  = Array.fill(colCount, rowCount)(BadCoordinate2D)
    otherBreadcrumbs :: HNil
  }

  override protected def mixinExtras(stepData: AStarStepData, extras: Extras) : BiDirStepData = {
    import stepData._
    extras match {
      case otherBreadcrumbs :: HNil => new BiDirStepData(loc, goal, beenThereArr, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr, otherBreadcrumbs, iters, endGoal)
      case _                        => throw new UnexpectedDataException("Malformed HList!")
    }
  }

}
