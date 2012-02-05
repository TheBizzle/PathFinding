package tester.test.suitereporter

import org.scalatest.Suite

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:02 PM
 */

abstract class SuiteCoagulator(reporters: SuiteReporter*) {
    def coagulate : Seq[Suite] = {
        SuiteReporter.coalesce(reporters)
    }
}
