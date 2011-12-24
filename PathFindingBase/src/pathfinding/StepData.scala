package pathfinding

import coordinate.Coordinate
import pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 5:27 PM
 */

abstract class StepData(currentLocation: Coordinate,
                        goalLocation: Coordinate,
                        pMap: PathingMap,
                        breadcrumbs: Array[Array[Coordinate]],
                        endGoalLocation: Coordinate = null) {

    val loc = currentLocation
    val goal = goalLocation
    val pathingMap = pMap
    val breadcrumbArr = breadcrumbs
    val endGoal = if (null == endGoalLocation) goalLocation else endGoalLocation    // This is a variable that is useful for A* algorithms that
                                                                                    // might have multiple intermediary goals,
                                                                                    // or for the backwards-moving track of bidirectional A*

}

trait StepDataSingleton[T] {
    def apply(freshLoc: Coordinate, stepData: T) : T
}