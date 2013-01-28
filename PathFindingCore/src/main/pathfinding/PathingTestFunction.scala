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

class PathingTestFunction(testString:       PathingMapString,
                          analysisFunction: (PathingStatus[StepData], PathingAnalysisFlagBundle) => PathingAnalysisResultBundle,
                          testNumber:       Int,
                          shouldPass:       Boolean,
                          expectedLength:   Int)
    extends TestFunction[PathFinder[StepData], PathingMapString, PathingStatus[StepData],
                         PathingAnalysisFlagBundle, PathingAnalysisResultBundle](testString, analysisFunction, testNumber, shouldPass) {

  def apply(pathFinder: PathFinder[StepData], flags: TestFuncFlagBundle) : Boolean = {

    val analysisFlags = extractAnalysisFlags(flags)
    val bundle        = analysisFunc(pathFinder(testSubject), analysisFlags)

    bundle.wasSuccess && (bundle.path.length - 1) == expectedLength

  }

  protected def extractAnalysisFlags(flags: TestFuncFlagBundle) = new PathingAnalysisFlagBundle(flags.toggles)

}

case class PTFConstructionBundle(expectedPathLength: Int) extends TestFuncConstructionBundle
