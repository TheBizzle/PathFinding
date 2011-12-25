package pathfinding.tester.test.dummies

import pathfinding.{PathFinder}
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.statuses.{Failure, ExecutionStatus}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 2:36 PM
 */

object DummyPathFinder extends PathFinder[DummyStepData] {
    def apply(mapString: PathingMapString) : ExecutionStatus[DummyStepData] = Failure(new DummyStepData())
}