package tester.cluster

import
  tester.criteria.{ ToggleFlagManager, Talkative, TestToggleFlag }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/29/12
 * Time: 6:24 PM
 */

trait TestAnalyzer[T <: ExecutionStatus, U <: TestAnalysisFlagBundle, V <: TestAnalysisResultBundle] {
  protected def analyze(status: T, flags: U) : V
}

trait ExecutionStatus
trait TestAnalysisResultBundle

abstract class TestAnalysisFlagBundle(inToggles: Set[TestToggleFlag], extras: Set[TestToggleFlag] = Set()) extends ToggleFlagManager(inToggles, extras ++ Set(Talkative))
