package tester.criteria

import tester.exceptions.MysteriousDataException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 9:21 PM
 */

sealed trait TestingFlag                              // A marker trait
sealed trait TestRunningnessFlag extends TestingFlag  // A trait whose possessors will represent signs of about whether or not a test(s) should be run
sealed trait TestToggleFlag extends TestingFlag {     // Everytime something is made to inherit from this, it MUST be added to TestToggleFlagWrapper's FlagList
  implicit def flagToCriteria(that: TestToggleFlag) : TestCriteriaToggleFlag = {
    TestCriteriaToggleFlag(that)
  }
}

case object RunTest extends TestRunningnessFlag
case object SkipTest extends TestRunningnessFlag

case object Talkative extends TestToggleFlag             // Enables the "Here, let me draw that for you on the map!" thing in PathFindingCore tests; overall, gives tests permission to println
case object RunBaseTests extends TestToggleFlag          // Enables running the ScalaTest tests on the core data structures and such
case object SkipExternalTests extends TestToggleFlag     // Skips the running of any of the actual external (e.g. pathfinding) tests
case object StackTrace extends TestToggleFlag            // Signifies the desire to see stacktraces when tests fail as a result of throwing exceptions

// If you add a flag to this file, add it to TestingFlag.flags!!!!!!

object TestingFlag {
  val flags = List(Talkative, RunBaseTests, SkipExternalTests, StackTrace)
  def flipRunningness(flag: TestRunningnessFlag) : TestRunningnessFlag = {
    flag match {
      case RunTest  => SkipTest
      case SkipTest => RunTest
      case _        => throw new MysteriousDataException("Unknown TestRunningnessFlag for runningness flip!")
    }
  }
}
