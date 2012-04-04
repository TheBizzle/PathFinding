package tester.criteria.parser

import util.parsing.combinator.RegexParsers
import tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/17/12
 * Time: 11:13 PM
 */

object TestCriteriaParser extends RegexParsers {

  // ============== MISC ====================

  val flags = TestingFlag.flags
  val flagNames = flags.map(_.getClass.getName.replaceAll("""(\$$)|(.*\.)""", ""))  // Remove end-of-entry '$'s and all text before the last '.'

  implicit private def not2Flag(not: Option[String]) : TestRunningnessFlag = {
    not map (x => SkipTest) getOrElse (RunTest)
  }

  // The parser gets confused when there are TestCriteria with different top types
  // The code looks ugly if I do a bunch of `asInstanceOf[TestCriteria] calls`
  // This silly method alleviates both of those problems together
  private def upcast(p: Parser[TestCriteria]) = p

  // =============== PARSERS =================

  val Not = """~""".r
  val Sep = """;""".r
  val Num = """[1-9][0-9]*""".r
  val RangeSep = """,|(->)|-|:""".r

  def testingnessValue: Parser[TestRunningnessValue] = opt(Not) ~ Num ^^ {
    case not ~ num => TestRunningnessValue(num.toInt, not)
  }

  def testingnessRange: Parser[TestRunningnessRange] = opt(Not) ~ Num ~ (RangeSep ~> Num) ^^ {
    case not ~ start ~ end => TestRunningnessRange(start.toInt, end.toInt, not)
  }

  def otherFlag: Parser[TestCriteriaToggleFlag] = ("(?i)" + flagNames.map("(%s)".format(_)).mkString("|")).r ^^ {
    case name => TestCriteriaToggleFlag(flags.zip(flagNames).find(_._2.compareToIgnoreCase(name) == 0).get._1)
  }

  def criteria: Parser[List[TestCriteria]] =  rep1sep(testingnessRange | testingnessValue | otherFlag, Sep) ^^ {
    case criteria: List[TestCriteria] => criteria
  }

}
