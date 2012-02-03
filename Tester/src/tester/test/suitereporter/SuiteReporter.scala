package tester.test.suitereporter

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
    def coalesce(seqs: Seq[Seq[Suite]]) : Seq[Suite] = {
        seqs.flatten
    }
    implicit def reporterSeqToSuiteSeqSeq(reporters: Seq[SuiteReporter]) : Seq[Seq[Suite]] = {
        reporters map ( _.report )
    }
}