package pathfinding.testcluster

import pathfinding.{StepData, PathFinder}
import pathfinding.statuses.PathingStatus
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import tester.testcluster.{TestFuncFlagBundle, TestFunction}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 9:58 PM
 */

class PathingTestFunction(testString: PathingMapString, analysisFunction: (PathingStatus[StepData], PathingAnalysisFlagBundle) => Boolean, testNumber: Int, shouldPass: Boolean)
                          extends TestFunction[PathFinder[StepData], PathingMapString, PathingStatus[StepData], PathingAnalysisFlagBundle](testString, analysisFunction, testNumber, shouldPass) {

    def apply(pathFinder: PathFinder[StepData], flags: TestFuncFlagBundle) : Boolean = {
        val analysisFlags = extractAnalysisFlags(flags)
        analysisFunc(pathFinder(testSubject), analysisFlags)
    }

    protected def extractAnalysisFlags(flags: TestFuncFlagBundle) : PathingAnalysisFlagBundle = {
        new PathingAnalysisFlagBundle(flags.getAll)
    }
    
}