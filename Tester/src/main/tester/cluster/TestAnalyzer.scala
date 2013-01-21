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

abstract class TestAnalysisResultBundle

abstract class TestAnalysisFlagBundle(inToggles: Seq[TestToggleFlag], extras: Seq[TestToggleFlag] = Seq())(implicit passItOn: Seq[TestToggleFlag] = extras ++ Seq(Talkative))
  extends ToggleFlagManager(inToggles, passItOn)
