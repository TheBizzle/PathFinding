package pathfinding

import pathingmap.pathingmapdata.PathingMapString
import statuses.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 5:09 PM
 */

trait PathFinder[T <: StepData] {
    def apply(mapString: PathingMapString) : ExecutionStatus[T]
}