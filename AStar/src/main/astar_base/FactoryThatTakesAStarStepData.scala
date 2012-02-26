package astar_base

import datastructure.priorityqueue.PriorityQueue
import pathfinding.pathingmap.PathingMap
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.coordinate.{PriorityCoordinateOrdering, PriorityCoordinate, Coordinate}
import shapeless._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/14/12
 * Time: 7:08 PM
 */

trait FactoryThatTakesAStarStepData[T] {

    // A bad/initial numerical value for some things—NOT COUNTING THE "X" AND "Y" MEMBERS OF COORDINATE OBJECTS; they have their own InvalidValue
    protected val BadVal = -1

    type Extras <: HList

    def apply(mapString: PathingMapString) : T = {
        
        val (start, goal, pathingMap) = PathingMap(mapString)

        val colCount = pathingMap.colCount
        val rowCount = pathingMap.rowCount

        val beenThere = Array.fill(colCount, rowCount)(false)
        val queue = new PriorityQueue[PriorityCoordinate](PriorityCoordinateOrdering.compare)

        val costArr = Array.fill(colCount, rowCount)(BadVal)
        val heuristicArr = Array.fill(colCount, rowCount)(BadVal)
        val totalArr = Array.fill(colCount, rowCount)(BadVal)
        val breadcrumbArr = Array.fill(colCount, rowCount)(Coordinate())

        val stepData = new AStarStepData(start, goal, beenThere, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr)
        val extras = generateExtras(stepData)
        mixinExtras(stepData, extras)

    }

    protected def generateExtras(stepData: AStarStepData) : Extras
    
    protected def mixinExtras(stepData: AStarStepData, extras: Extras) : T

}
