package pathfinding.testcluster

import tester.testcluster.TestFunction
import pathfinding.{StepData, PathFinder}
import pathfinding.statuses.PathingStatus
import pathfinding.pathingmap.pathingmapdata.PathingMapString

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 9:58 PM
 */

class PathingTestFunction(testString: PathingMapString, analysisFunction: (PathingStatus[StepData], PathingAnalysisFlagBundle) => Boolean, testNumber: Int, shouldPass: Boolean)
                          extends TestFunction[PathFinder[StepData], PathingMapString, PathingStatus[StepData], PathingTestFlagBundle, PathingAnalysisFlagBundle](testString, analysisFunction, testNumber, shouldPass) {

    def apply(pathFinder: PathFinder[StepData], flags: PathingTestFlagBundle) : Boolean = {
        val analysisFlags = extractAnalysisFlags(flags)
        analysisFunc(pathFinder(testSubject), analysisFlags)
    }

    protected def extractAnalysisFlags(flags: PathingTestFlagBundle) : PathingAnalysisFlagBundle = {
        new PathingAnalysisFlagBundle(flags.getAnalysisFlags)
    }
    
}