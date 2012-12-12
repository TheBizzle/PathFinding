package tester.suitereporter

import tester.{TesterFunSuite, TesterSpec}
import tester.criteria.parser.CriteriaParserFunSuite
import tester.testscript.dialect.CriteriaDialectFunSuite

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:51 PM
 */

object TesterSuiteReporter extends SuiteReporter(Seq(new TesterFunSuite(), new TesterSpec(), new CriteriaParserFunSuite(), new CriteriaDialectFunSuite()))
