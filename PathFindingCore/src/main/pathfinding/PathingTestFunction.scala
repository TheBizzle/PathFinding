package pathfinding

import
  tester.cluster.{ TestFuncConstructionBundle, TestFuncFlagBundle, TestFunction }

import
  pathingmap.PathingMapString

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
    new PathingAnalysisFlagBundle(flags.toggles)
  }

}

class PTFConstructionBundle(val expectedPathLength: Int) extends TestFuncConstructionBundle
