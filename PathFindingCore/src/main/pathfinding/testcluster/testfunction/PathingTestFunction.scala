package pathfinding.testcluster.testfunction

import pathfinding.{StepData, PathFinder}
import pathfinding.statuses.PathingStatus
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import tester.testcluster.testfunction.{TestFuncFlagBundle, TestFunction}
import pathfinding.testanalyzer.{PathingAnalysisFlagBundle, PathingAnalysisResultBundle}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 9:58 PM
 */

class PathingTestFunction(testString: PathingMapString,
                          analysisFunction: (PathingStatus[StepData], PathingAnalysisFlagBundle) => PathingAnalysisResultBundle,
                          testNumber: Int,
                          shouldPass: Boolean,
                          expectedLength: Int)
                          extends TestFunction[PathFinder[StepData], PathingMapString, PathingStatus[StepData],
                                               PathingAnalysisFlagBundle, PathingAnalysisResultBundle](testString, analysisFunction, testNumber, shouldPass) {

    val expectedPathLength = expectedLength

    def apply(pathFinder: PathFinder[StepData], flags: TestFuncFlagBundle) : Boolean = {
        val analysisFlags = extractAnalysisFlags(flags)
        val bundle = analysisFunc(pathFinder(testSubject), analysisFlags)
        if (!bundle.wasSuccess)
            false
        else
            (bundle.path.length - 1) == expectedPathLength
    }

    protected def extractAnalysisFlags(flags: TestFuncFlagBundle) : PathingAnalysisFlagBundle = {
        new PathingAnalysisFlagBundle(flags.getAll)
    }
    
}
