package tester.suite

import org.scalatest.Suite

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:50 PM
 */

abstract class SuiteReporter(suites: Seq[Suite]) {
  def report : Seq[Suite] = suites
}

object SuiteReporter {
  def coalesce(seqs: Seq[Seq[Suite]]) : Seq[Suite] = seqs.flatten
  implicit def reporterSeq2SuiteSeqSeq(reporters: Seq[SuiteReporter]) = reporters map (_.report)
}

abstract class SuiteCoagulator(reporters: SuiteReporter*) {
  def coagulate : Seq[Suite] = {
    SuiteReporter.coalesce(reporters)
  }
}
