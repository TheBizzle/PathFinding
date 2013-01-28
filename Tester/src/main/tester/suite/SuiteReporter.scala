package tester.suite

import
  org.scalatest.Suite

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:50 PM
 */

class SuiteReporter[+T <: Suite](val suites: Seq[T] = Seq[Suite]()) {
  def ++[U >: T <: Suite](that: SuiteReporter[U]) : SuiteReporter[U] = new SuiteReporter[U](suites ++ that.suites)
}
