package astar_base

import datastructure.priorityqueue.PriorityQueue
import pathfinding.pathingmap.PathingMap
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.coordinate.{PriorityCoordinateOrdering, PriorityCoordinate, Coordinate}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/14/12
 * Time: 7:08 PM
 */

trait FactoryThatTakesAStarStepData[T] {

    // A bad/initial numerical value for some things—NOT COUNTING THE "X" AND "Y" MEMBERS OF COORDINATE OBJECTS; they have their own InvalidValue
    protected val BadVal = -1

    def apply(mapString: PathingMapString) : T = {
        
        val (start, goal, pathingMap) = PathingMap(mapString)

        val colCount = pathingMap.colCount
        val rowCount = pathingMap.rowCount

        val beenThere = initialize2DArr(colCount, rowCount, false)
        val queue = new PriorityQueue[PriorityCoordinate](PriorityCoordinateOrdering.compare)

        val costArr = initialize2DArr(colCount, rowCount, BadVal)
        val heuristicArr = initialize2DArr(colCount, rowCount, BadVal)
        val totalArr = initialize2DArr(colCount, rowCount, BadVal)
        val breadcrumbArr = initialize2DArr(colCount, rowCount, new Coordinate())

        val stepData = new AStarStepData(start, goal, beenThere, queue, pathingMap, costArr, heuristicArr, totalArr, breadcrumbArr)
        val extras = generateExtras(stepData)
        mixinExtras(stepData, extras)

    }

    protected def initialize2DArr[T: Manifest](cols: Int, rows: Int, defaultVal: T) : Array[Array[T]] = {
        new Array[Array[T]](cols) map { case x => new Array[T](rows) map (y => defaultVal) }
    }

    protected def generateExtras(stepData: AStarStepData) : Seq[Any] = {
        Nil
    }
    
    protected def mixinExtras(stepData: AStarStepData, extras: Seq[Any]) : T

}
