package tester

import
  util.parsing.combinator.RegexParsers

import
  tester.criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 3/17/12
 * Time: 11:13 PM
 */

// This was a fun experiment, but it is actually no longer used in the code
// It has been replaced by TestCriteriaDialect
object TestCriteriaParser extends RegexParsers {

  // ============== MISC ====================

  val flags     = TestingFlag.flags
  val flagNames = flags.map(_.getClass.getName.replaceAll("""(\$$)|(.*\.)""", ""))  // (i.e. "tester.criteria.RunTest$" => "RunTest")

  implicit private def not2Flag(not: Option[String]) : TestRunningnessFlag = not map (x => SkipTest) getOrElse (RunTest)

  // =============== PARSERS =================

  val Not         = """~|!""".r
  val Sep         = """;|&&""".r
  val Num         = """[1-9][0-9]*""".r
  val RangeSep    = """,|(->)|(>&>)|-|:""".r
  val NotRangeSep = """>!>""".r

  def testingnessValue : Parser[TestRunningnessValue] = opt(Not) ~ Num ^^ {
    case not ~ num => TestRunningnessValue(num.toInt, not)
  }

  def testingnessRange : Parser[TestRunningnessRange] = opt(Not) ~ Num ~ (RangeSep ~> Num) ^^ {
    case not ~ start ~ end => TestRunningnessRange(start.toInt, end.toInt, not)
  }

  // Added for compatibility with TestCriteriaDialect
  def testingnessNotRange : Parser[TestRunningnessRange] = Num ~ (NotRangeSep ~> Num) ^^ {
    case start ~ end => TestRunningnessRange(start.toInt, end.toInt, SkipTest)
  }

  def otherFlag : Parser[TestCriteriaToggleFlag] = ("(?i)" + flagNames.map("(%s)".format(_)).mkString("|")).r ^^ {
    case name => TestCriteriaToggleFlag(flags.zip(flagNames).find(_._2.compareToIgnoreCase(name) == 0).get._1)
  }

  def crit : Parser[Seq[TestCriteria]] = rep1sep(testingnessNotRange | testingnessRange | testingnessValue | otherFlag, Sep) ^^ {
    case criteria: Seq[TestCriteria] => criteria
  }

}
