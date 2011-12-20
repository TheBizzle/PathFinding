package astar_base.tester

import coordinate.Coordinate
import pathingmap.PathingMap
import astar_base.statuses._
import astar_base.StepData

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/18/11
 * Time: 2:01 AM
 */

object PathingTestAnalyzer {

    def apply(status: ExecutionStatus[StepData]) {
        status match {
            case Success(x) => { println("Success!"); retracePath(x.breadcrumbArr, x.endGoal, x.pathingMap) }
            case _ => println("Failed to find a solution....\n\n")
        }
    }

    private def retracePath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate, pathingMap: PathingMap) {

        val pathTaken = eatBreadcrumbsForPath(breadcrumbs, goal)
        println("The path taken was: " + pathTaken + "\nHere, let me draw that for you on the map!\n")

        pathingMap.markAsGoal(goal)
        println(PathingMap.generateCloneWithPath(pathTaken, pathingMap).toString)

        val suggestedLoc = pathTaken.tail.head
        println("So, anyway... you should move " + PathingMap.findDirection(pathTaken.head, suggestedLoc) + " towards " + suggestedLoc + "\n\n")

    }

    private def eatBreadcrumbsForPath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate) : List[Coordinate] = {
        def breadcrumbsHelper(breadcrumbs: Array[Array[Coordinate]], current: Coordinate) : List[Coordinate] = {
            val next = breadcrumbs(current.x)(current.y)
            current :: {
                if ((next.x != Coordinate.InvalidValue) && (next.y != Coordinate.InvalidValue))
                    breadcrumbsHelper(breadcrumbs, next)
                else
                    Nil
            }
        }
        breadcrumbsHelper(breadcrumbs, breadcrumbs(goal.x)(goal.y)).reverse
    }

}