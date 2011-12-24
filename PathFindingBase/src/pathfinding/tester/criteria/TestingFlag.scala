package pathfinding.tester.criteria

import java.security.InvalidParameterException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 9:21 PM
 */

sealed trait TestingFlag                             // Marker traits

sealed trait RunningnessTestingFlag
sealed trait TestRunFlag extends RunningnessTestingFlag
sealed trait TestSkipFlag extends RunningnessTestingFlag

sealed trait ArityTestingFlag
sealed trait TestRangeFlag extends ArityTestingFlag
sealed trait TestValueFlag extends ArityTestingFlag

sealed trait TestToggleFlag extends TestingFlag      // Everytime something is made to inherit from this, it MUST be added to TestToggleFlagWrapper's FlagList

object RunRange extends TestRangeFlag with TestRunFlag
object SkipRange extends TestRangeFlag with TestSkipFlag

object RunTest extends TestValueFlag with TestRunFlag
object SkipTest extends TestValueFlag with TestSkipFlag

case object Talkative extends TestToggleFlag             // Enables the "Here, let me draw that for you on the map!" thing
case object RunBaseTests extends TestToggleFlag          // Enables running the ScalaTest tests on the core data structures and such
case object SkipPathingTests extends TestToggleFlag      // Skips the running of any of the actual pathfinding tests

object TestingFlag {

    // The things that I do to appease you, Scala...
    def flipRunningness(flag: TestRunFlag) : TestSkipFlag = {
        flag match {
            case x: TestValueFlag => SkipTest
            case x: TestRangeFlag => SkipRange
            case _                => throw new InvalidParameterException("Unknown arity on runningness flag flip!")
        }
    }

    def flipRunningness(flag: TestSkipFlag) : TestRunFlag = {
        flag match {
            case x: TestValueFlag => RunTest
            case x: TestRangeFlag => RunRange
            case _                => throw new InvalidParameterException("Unknown arity on runningness flag flip!")
        }
    }

    def flipArity(flag: TestRangeFlag) : TestValueFlag = {
        flag match {
            case x: TestRunFlag  => RunTest
            case x: TestSkipFlag => SkipTest
            case _               => throw new InvalidParameterException("Unknown runningness on arity flag flip!")
        }
    }

    def flipArity(flag: TestValueFlag) : TestRangeFlag = {
        flag match {
            case x: TestRunFlag  => RunRange
            case x: TestSkipFlag => SkipRange
            case _               => throw new InvalidParameterException("Unknown runningness on arity flag flip!")
        }
    }

}