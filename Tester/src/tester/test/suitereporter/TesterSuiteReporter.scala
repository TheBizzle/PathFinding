package tester.test.suitereporter

import tester.test.{TesterFunSuite, TesterSpec}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:51 PM
 */

object TesterSuiteReporter extends SuiteReporter(List(new TesterFunSuite(), new TesterSpec()))
