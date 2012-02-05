package tester.testanalyzer

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/29/12
 * Time: 6:24 PM
 */

trait TestAnalyzer[T <: ExecutionStatus, U <: TestAnalysisFlagBundle] {
    protected def analyze(status: T, flags: U) : Boolean
}
