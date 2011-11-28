package com.AStar.astar

//import collection.mutable.PriorityQueue
import com.AStar.datastructure.priorityqueue.PriorityQueue
import com.AStar.pathingmap._
import com.AStar.coordinate._

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
               breadcrumbs: Array[Array[Coordinate]]) {

    val loc = currentLocation
    val goal = goalLocation
    val beenThereArr = beenThere
    val queue = pQueue
    val pathingMap = pMap
    val costArr = costArray
    val heuristicArr = heuristicArray
    val totalArr = totalArray
    val breadcrumbArr = breadcrumbs

}