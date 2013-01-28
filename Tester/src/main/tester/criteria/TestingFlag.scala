package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 9:21 PM
 */

sealed trait TestingFlag
object TestingFlag {
  val flags = Set[TestToggleFlag](Talkative, RunBaseTests, SkipExternalTests, StackTrace)
}



sealed trait TestToggleFlag extends TestingFlag {
  implicit def flag2Criteria(that: TestToggleFlag) = TestCriteriaToggleFlag(that)
}

case object Talkative         extends TestToggleFlag  // Enables the "Here, let me draw that for you on the map!" thing in PathFindingCore tests; overall, gives tests permission to println
case object RunBaseTests      extends TestToggleFlag  // Enables running the ScalaTest tests on the core data structures and such
case object SkipExternalTests extends TestToggleFlag  // Skips the running of any of the actual external (e.g. pathfinding) tests
case object StackTrace        extends TestToggleFlag  // Signifies the desire to see stacktraces when tests fail as a result of throwing exceptions



sealed trait TestRunningnessFlag extends TestingFlag {
  protected def isRunning : Boolean
  /*none */ def flip      : TestRunningnessFlag = TestRunningnessFlag(!isRunning)
}

object TestRunningnessFlag {
  def apply(isRunning: Boolean) = if (isRunning) RunTest else SkipTest
}

case object RunTest extends TestRunningnessFlag {
  override protected def isRunning = true
}
case object SkipTest extends TestRunningnessFlag {
  override protected def isRunning = false
}

