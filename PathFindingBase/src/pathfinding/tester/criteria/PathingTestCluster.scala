package pathfinding.tester.criteria

import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.{StepData, PathFinder}
import pathfinding.statuses.{Success, ExecutionStatus}
import pathfinding.coordinate.Coordinate
import pathfinding.pathingmap.PathingMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 4:28 PM
 */

sealed abstract class TestFunction extends Function2[PathFinder, Boolean, Unit] {
    // Consider not passing the toggle flags to apply() and just batch-processing toggles into function object local variables.
    // That way, the signatures of the functions wouldn't need to change if ever more toggles were added.
    // However... maybe they should HAVE TO change (to be sure of compliance).... :food for thought:
    def apply(pathFinder: PathFinder, isTalkative: Boolean)
}

object PathingTestCluster {

    // I hope that calling (object Array).apply(args) will only make an array that has args.length elements...
    private val tests = Array[TestFunction](Test1, Test2, Test3, Test4, Test5, Test6)

    def runTests(testNums: List[Int], thingToTest: PathFinder, isTalkative: Boolean) {
        testNums foreach ( tests(_)(thingToTest, isTalkative) )
    }

    def getSize : Int = {
        tests.length
    }

    private def analyze(status: ExecutionStatus[StepData], isTalkative: Boolean) {
        status match {
            case Success(x) => { if (isTalkative) println("Success!"); retracePath(x.breadcrumbArr, x.endGoal, x.pathingMap, isTalkative) }
            case _ => if (isTalkative) println("Failed to find a solution....\n\n")
        }
    }

    private def retracePath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate, pathingMap: PathingMap, isTalkative: Boolean) {

        val pathTaken = eatBreadcrumbsForPath(breadcrumbs, goal)

        if (isTalkative)
            println("The path taken was: " + pathTaken + "\nHere, let me draw that for you on the map!\n")

        pathingMap.markAsGoal(goal)

        if (isTalkative)
            println(PathingMap.generateCloneWithPath(pathTaken, pathingMap).toString)

        val suggestedLoc = pathTaken.tail.head

        if (isTalkative)
            println("So, anyway... you should move " + PathingMap.findDirection(pathTaken.head, suggestedLoc) + " towards " + suggestedLoc + "\n\n")

    }

    private def eatBreadcrumbsForPath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate) : List[Coordinate] = {
        def breadcrumbsHelper(breadcrumbs: Array[Array[Coordinate]], current: Coordinate) : List[Coordinate] = {
            current :: {
                val next = breadcrumbs(current.x)(current.y)
                next match {
                    case Coordinate() => Nil
                    case _            => breadcrumbsHelper(breadcrumbs, next)
                }
            }
        }
        breadcrumbsHelper(breadcrumbs, breadcrumbs(goal.x)(goal.y)).reverse
    }

    // =====================================+---------------------------------+======================================
    //                                      |   Code for tests starts here    |
    // =====================================+---------------------------------+======================================

    private object Test1 extends TestFunction {
        def apply(pathFinder: PathFinder, isTalkative: Boolean) {
            analyze(pathFinder(TestMapString1), isTalkative)
        }
    }

    private object Test2 extends TestFunction {
        def apply(pathFinder: PathFinder, isTalkative: Boolean) {
            analyze(pathFinder(TestMapString2), isTalkative)
        }
    }

    private object Test3 extends TestFunction {
        def apply(pathFinder: PathFinder, isTalkative: Boolean) {
            analyze(pathFinder(TestMapString3), isTalkative)
        }
    }

    private object Test4 extends TestFunction {
        def apply(pathFinder: PathFinder, isTalkative: Boolean) {
            analyze(pathFinder(TestMapString4), isTalkative)
        }
    }

    private object Test5 extends TestFunction {
        def apply(pathFinder: PathFinder, isTalkative: Boolean) {
            analyze(pathFinder(TestMapString5), isTalkative)
        }
    }

    private object Test6 extends TestFunction {
        def apply(pathFinder: PathFinder, isTalkative: Boolean) {
            analyze(pathFinder(TestMapString6), isTalkative)
        }
    }

    // ==============================+--------------------------------------------+==================================
    //                                |   Code for test map strings starts here    |
    // ==============================+--------------------------------------------+==================================

    object TestMapString1 extends PathingMapString("*_____________G", "akjshdkjashldjaksdhljakds")

    object TestMapString2 extends PathingMapString("_*asdf" +
                                                   "G_asdf", "asdf")
    // Should time out and break
    object TestMapString3 extends PathingMapString("_%__*|" +
                                                   "OG%_%|" +
                                                   "%%___|", "\\|")

    object TestMapString4 extends PathingMapString("_%__*|" +
                                                   "OG%_%|" +
                                                   "%____|", "\\|")

    object TestMapString5 extends PathingMapString("_______________|" +
                                                   "___________*___|" +
                                                   "_______________|" +
                                                   "_______________|" +
                                                   "%%%%%%%%%%_____|" +
                                                   "________GD_____|" +
                                                   "D_DDDDDDDD_____|" +
                                                   "__D_D____D_____|" +
                                                   "_DD______D_____|" +
                                                   "____D_DDDD_____|" +
                                                   "DDDDD____D_____|" +
                                                   "____DDDD_D_____|" +
                                                   "_______________|" +
                                                   "_______________|" +
                                                   "_______________", "\\|")

    object TestMapString6 extends PathingMapString("_______________|" +
                                                   "___________*___|" +
                                                   "_________O%%%%%|" +
                                                   "_______________|" +
                                                   "%%%%%%%%%%%%%%_|" +
                                                   "________GD_____|" +
                                                   "D_DDDDDDDD_%%%%|" +
                                                   "__D_D____D_____|" +
                                                   "_DD______D%%%%_|" +
                                                   "____D_DDDD_____|" +
                                                   "DDDDD____D_____|" +
                                                   "____DDDD_D_____|" +
                                                   "_______%_%_____|" +
                                                   "_______%_%_____|" +
                                                   "_______________", "\\|")

}