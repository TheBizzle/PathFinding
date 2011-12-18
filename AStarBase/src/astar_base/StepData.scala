package astar_base

//import collection.mutable.PriorityQueue
import coordinate.{PriorityCoordinate, Coordinate}
import datastructure.priorityqueue.PriorityQueue
import pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 10:46 PM
 */
class StepData(currentLocation: Coordinate,
               goalLocation: Coordinate,
               beenThere: Array[Array[Boolean]],
               pQueue: PriorityQueue[PriorityCoordinate],
               pMap: PathingMap,
               costArray: Array[Array[Int]],
               heuristicArray: Array[Array[Int]],
               totalArray: Array[Array[Int]],
               breadcrumbs: Array[Array[Coordinate]],
               endGoalLocation: Coordinate = null) {

    val loc = currentLocation
    val goal = goalLocation
    val beenThereArr = beenThere
    val queue = pQueue
    val pathingMap = pMap
    val costArr = costArray
    val heuristicArr = heuristicArray
    val totalArr = totalArray
    val breadcrumbArr = breadcrumbs
    val endGoal = if (null == endGoalLocation) goalLocation else endGoalLocation    // This is a variable that is useful for A* algorithms that\
                                                                                    // might have multiple intermediary goals,
                                                                                    // or for the backwards-moving track of bidirectional A*

}

object StepData {
    def apply(freshLoc: Coordinate, stepData: StepData) : StepData = {
        import stepData._
        new StepData(freshLoc, goal, beenThereArr, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr)
    }
}