package pathfinding.tester.test.dummies

import pathfinding.{StepData, StepDataSingleton}
import pathfinding.coordinate.Coordinate


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 2:37 PM
 */

class DummyStepData extends StepData(Coordinate(), Coordinate(), null, null)

object DummyStepData extends StepDataSingleton[DummyStepData] {
    def apply(freshLoc: Coordinate, stepData: DummyStepData) : DummyStepData = {
        new DummyStepData()
    }
}