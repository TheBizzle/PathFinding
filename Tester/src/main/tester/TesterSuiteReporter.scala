package tester

import suite.SuiteReporter

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/16/12
 * Time: 11:09 AM
 */

object TesterSuiteReporter extends SuiteReporter(Seq(new TesterFunSuite(), new TesterSpec(), new CriteriaParserFunSuite(), new CriteriaDialectFunSuite()))

