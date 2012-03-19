package tester.suitereporter

import tester.{TesterFunSuite, TesterSpec}
import tester.criteria.parser.CriteriaParserFunSuite

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:51 PM
 */

object TesterSuiteReporter extends SuiteReporter(List(new TesterFunSuite(), new TesterSpec(), new CriteriaParserFunSuite()))
