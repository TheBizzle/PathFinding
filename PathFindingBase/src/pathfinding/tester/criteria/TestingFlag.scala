package pathfinding.tester.criteria

import java.security.InvalidParameterException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 9:21 PM
 */

sealed trait TestingFlag                             // Marker traits
sealed trait TestToggleFlag extends TestingFlag      // Everytime something is made to inherit from this, it MUST be added to TestToggleFlagWrapper's FlagList
sealed trait TestRunningnessFlag extends TestingFlag

case object RunTest extends TestRunningnessFlag
case object SkipTest extends TestRunningnessFlag

case object Talkative extends TestToggleFlag             // Enables the "Here, let me draw that for you on the map!" thing
case object RunBaseTests extends TestToggleFlag          // Enables running the ScalaTest tests on the core data structures and such
case object SkipPathingTests extends TestToggleFlag      // Skips the running of any of the actual pathfinding tests

object TestingFlag {

    // The things that I do to appease you, Scala...
    def flipRunningness(flag: TestRunningnessFlag) : TestRunningnessFlag = {
        flag match {
            case RunTest  => SkipTest
            case SkipTest => RunTest
            case _    => throw new InvalidParameterException("Unknown TestRunningnessFlag for runningness flip!")
        }
    }

}