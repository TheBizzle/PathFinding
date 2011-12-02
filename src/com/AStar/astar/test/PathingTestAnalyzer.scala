package com.AStar.astar.test

import com.AStar.astar.StepData
import com.AStar.coordinate.Coordinate
import com.AStar.pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/18/11
 * Time: 2:01 AM
 */

object PathingTestAnalyzer {

    def apply(stepData: StepData) {

        import stepData._

        if ((loc.x > -1) && (loc.y > -1)) {
            println("Success!")
            retracePath(breadcrumbArr, goal, pathingMap)
        }
        else
            println("Failed to find a solution....\n\n")
        
    }

    private def retracePath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate, pathingMap: PathingMap) {

        val pathTaken = eatBreadcrumbsForPath(breadcrumbs, goal)
        println("The path taken was: " + pathTaken + "\nHere, let me draw that for you on the map!\n")

        println(PathingMap.generateCloneWithPath(pathTaken, pathingMap).toString)

        val suggestedLoc = pathTaken.tail.head
        println("So, anyway... you should move " + PathingMap.findDirection(pathTaken.head, suggestedLoc) + " towards " + suggestedLoc + "\n\n")

    }

    private def eatBreadcrumbsForPath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate) : List[Coordinate] = {
        def breadcrumbsHelper(breadcrumbs: Array[Array[Coordinate]], current: Coordinate) : List[Coordinate] = {
            val next = breadcrumbs(current.x)(current.y)
            current :: {
                if ((next.x > -1) && (next.y > -1))
                    breadcrumbsHelper(breadcrumbs, next)
                else
                    Nil
            }
        }
        breadcrumbsHelper(breadcrumbs, breadcrumbs(goal.x)(goal.y)).reverse
    }

}