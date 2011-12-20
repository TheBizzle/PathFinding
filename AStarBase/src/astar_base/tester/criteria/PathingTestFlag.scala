package astar_base.tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 9:21 PM
 */

sealed trait PathingTestFlag extends Enumeration        // Marker trait

object PathingTestRangeFlag extends PathingTestFlag {
    val RunRangeTrue, RunRangeFalse     // Marks a range of tests to run/skip
        = Value
}

object PathingTestValueFlag extends PathingTestFlag {
    val RunTestTrue, RunTestFalse       // Marks a specific test to run/skip
        = Value
}

object PathingTestToggleFlag extends PathingTestFlag {
    val TalkativeTrue,                  // Enables the "Here, let me draw that for you on the map!" thing
        RunBaseTestsTrue,               // Enables running the ScalaTest tests on the core data structures and such
        RunOnlyBaseTestsTrue            // Skips the running of any of the actual pathfinding tests
        = Value
}
