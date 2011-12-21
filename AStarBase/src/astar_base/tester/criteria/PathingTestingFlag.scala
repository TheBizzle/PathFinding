package astar_base.tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 9:21 PM
 */

sealed trait PathingTestingFlag                                             // Marker traits
sealed trait TestRunFlag extends PathingTestingFlag
sealed trait TestSkipFlag extends PathingTestingFlag
sealed trait TestRangeFlag extends PathingTestingFlag
sealed trait TestValueFlag extends PathingTestingFlag
sealed trait TestToggleFlag extends PathingTestingFlag                      // Everytime something is made to inherit from this,
                                                                            // it MUST be added to TestToggleFlagWrapper's FlagList

object RunRange extends TestRangeFlag with TestRunFlag
object SkipRange extends TestRangeFlag with TestSkipFlag

object RunTest extends TestValueFlag with TestRunFlag
object SkipTest extends TestValueFlag with TestSkipFlag

case object Talkative extends TestToggleFlag             // Enables the "Here, let me draw that for you on the map!" thing
case object RunBaseTests extends TestToggleFlag          // Enables running the ScalaTest tests on the core data structures and such
case object SkipPathingTests extends TestToggleFlag      // Skips the running of any of the actual pathfinding tests
