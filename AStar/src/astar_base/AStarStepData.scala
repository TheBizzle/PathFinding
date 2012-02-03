package astar_base

//import collection.mutable.PriorityQueue
import datastructure.priorityqueue.PriorityQueue
import pathfinding.coordinate.{PriorityCoordinate, Coordinate}
import pathfinding.pathingmap.PathingMap
import pathfinding.{StepData, StepDataSingleton}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 10:46 PM
 */
class AStarStepData(currentLocation: Coordinate,
                    goalLocation: Coordinate,
                    beenThere: Array[Array[Boolean]],
                    pQueue: PriorityQueue[PriorityCoordinate],
                    pMap: PathingMap,
                    costArray: Array[Array[Int]],
                    heuristicArray: Array[Array[Int]],
                    totalArray: Array[Array[Int]],
                    breadcrumbs: Array[Array[Coordinate]],
                    iterationCount: Int = 0,
                    endGoalLocation: Coordinate = null) extends StepData(currentLocation, goalLocation, pMap, breadcrumbs, endGoalLocation) {

    val beenThereArr = beenThere
    val queue = pQueue
    val costArr = costArray
    val heuristicArr = heuristicArray
    val totalArr = totalArray
    private var ic = iterationCount     // Satan's var

    def iters : Int = ic

    def incIters() {
        ic = ic + 1
    }

}

object AStarStepData extends StepDataSingleton[AStarStepData] with FactoryThatTakesAStarStepData[AStarStepData] {

    def apply(freshLoc: Coordinate, stepData: AStarStepData) : AStarStepData = {
        import stepData._
        new AStarStepData(freshLoc, goal, beenThereArr, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr, iters)
    }

    override protected def mixinExtras(stepData: AStarStepData, extras: Seq[Any]) : AStarStepData = {
        stepData
    }

}
