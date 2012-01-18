package pathfinding.test

import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.pathingmap.PathingMap
import tester.testcluster.{TestCluster, TestFunction}
import pathfinding.statuses.{Failure, Success, ExecutionStatus}
import pathfinding.{PathFinder, StepData}
import pathfinding.coordinate.{PriorityCoordinate, Coordinate}
import tester.exceptions.MysteriousDataException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 4:28 PM
 */

// If I see that fucking non-variant StepData one more time...!
object PathingTestCluster extends TestCluster[PathFinder[StepData]] {

    // There is no test #0—IS NOT AND NEVER WILL BE
    private val tests = Array[TestFunction[PathFinder[StepData]]](null, Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8, Test9, Test10, Test11, Test12, Test13, Test14, Test15,
                                                                        Test16, Test17, Test18, Test19, Test20, Test21, Test22, Test23, Test24, Test25, Test26, Test27, Test28, Test29, Test30,
                                                                        Test31, Test32, Test33, Test34, Test35, Test36, Test37, Test38, Test39)

    def getTestsToRun(testNums: List[Int]) : List[TestFunction[PathFinder[StepData]]] = {
        tests.zipWithIndex.filter { case(x, y) => testNums.contains(y) }.map ( _._1 ).toList
    }

    def getSize : Int = {
        tests.length - 1
    }

    private def analyze[T <: StepData](status: ExecutionStatus[T], isTalkative: Boolean) : Boolean = {

        status match {
            case Success(x) => {
                if (isTalkative) println("Found a solution!")
                retracePath(x.breadcrumbArr, x.endGoal, x.pathingMap, isTalkative)
                true
            }
            case Failure(_) => {
                if (isTalkative) println("Failed to find a solution....\n")
                false
            }
            case _          => throw new MysteriousDataException("Unexpected ExecutionStatus!")
        }

    }

    private def retracePath(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate, pathingMap: PathingMap, isTalkative: Boolean) {

        val pathTaken = eatBreadcrumbsForPath(breadcrumbs, goal).map { case x: PriorityCoordinate => x.asCoordinate
                                                                       case x                     => x }

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
                    case Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue) => Nil
                    case _                                                            => breadcrumbsHelper(breadcrumbs, next)
                }
            }
        }
        breadcrumbsHelper(breadcrumbs, breadcrumbs(goal.x)(goal.y)).reverse
    }

    // =====================================+---------------------------------+======================================
    // >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|   Code for tests starts here    |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    // =====================================+---------------------------------+======================================

    private object Test1 extends TestFunction[PathFinder[StepData]](1) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString1), isTalkative)
        }
    }

    private object Test2 extends TestFunction[PathFinder[StepData]](2) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString2), isTalkative)
        }
    }

    private object Test3 extends TestFunction[PathFinder[StepData]](3, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString3), isTalkative)
        }
    }

    private object Test4 extends TestFunction[PathFinder[StepData]](4) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString4), isTalkative)
        }
    }

    private object Test5 extends TestFunction[PathFinder[StepData]](5) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString5), isTalkative)
        }
    }

    private object Test6 extends TestFunction[PathFinder[StepData]](6) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString6), isTalkative)
        }
    }

    private object Test7 extends TestFunction[PathFinder[StepData]](7, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString7), isTalkative)
        }
    }

    private object Test8 extends TestFunction[PathFinder[StepData]](8) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString8), isTalkative)
        }
    }

    private object Test9 extends TestFunction[PathFinder[StepData]](9) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString9), isTalkative)
        }
    }

    private object Test10 extends TestFunction[PathFinder[StepData]](10) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString10), isTalkative)
        }
    }

    private object Test11 extends TestFunction[PathFinder[StepData]](11) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString11), isTalkative)
        }
    }

    private object Test12 extends TestFunction[PathFinder[StepData]](12) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString12), isTalkative)
        }
    }

    private object Test13 extends TestFunction[PathFinder[StepData]](13) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString13), isTalkative)
        }
    }

    private object Test14 extends TestFunction[PathFinder[StepData]](14) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString14), isTalkative)
        }
    }

    private object Test15 extends TestFunction[PathFinder[StepData]](15) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString15), isTalkative)
        }
    }

    private object Test16 extends TestFunction[PathFinder[StepData]](16) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString16), isTalkative)
        }
    }

    private object Test17 extends TestFunction[PathFinder[StepData]](17) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString17), isTalkative)
        }
    }

    private object Test18 extends TestFunction[PathFinder[StepData]](18) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString18), isTalkative)
        }
    }

    private object Test19 extends TestFunction[PathFinder[StepData]](19) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString19), isTalkative)
        }
    }

    private object Test20 extends TestFunction[PathFinder[StepData]](20) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString20), isTalkative)
        }
    }

    private object Test21 extends TestFunction[PathFinder[StepData]](21) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString21), isTalkative)
        }
    }

    private object Test22 extends TestFunction[PathFinder[StepData]](22) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString22), isTalkative)
        }
    }

    private object Test23 extends TestFunction[PathFinder[StepData]](23) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString23), isTalkative)
        }
    }

    private object Test24 extends TestFunction[PathFinder[StepData]](24) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString24), isTalkative)
        }
    }

    private object Test25 extends TestFunction[PathFinder[StepData]](25) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString25), isTalkative)
        }
    }

    private object Test26 extends TestFunction[PathFinder[StepData]](26) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString26), isTalkative)
        }
    }

    private object Test27 extends TestFunction[PathFinder[StepData]](27) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString27), isTalkative)
        }
    }

    private object Test28 extends TestFunction[PathFinder[StepData]](28) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString28), isTalkative)
        }
    }

    private object Test29 extends TestFunction[PathFinder[StepData]](29) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString29), isTalkative)
        }
    }

    private object Test30 extends TestFunction[PathFinder[StepData]](30) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString30), isTalkative)
        }
    }

    private object Test31 extends TestFunction[PathFinder[StepData]](31) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString31), isTalkative)
        }
    }

    private object Test32 extends TestFunction[PathFinder[StepData]](32) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString32), isTalkative)
        }
    }

    private object Test33 extends TestFunction[PathFinder[StepData]](33) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString33), isTalkative)
        }
    }

    private object Test34 extends TestFunction[PathFinder[StepData]](34) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString34), isTalkative)
        }
    }

    private object Test35 extends TestFunction[PathFinder[StepData]](35, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString35), isTalkative)
        }
    }

    private object Test36 extends TestFunction[PathFinder[StepData]](36, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString36), isTalkative)
        }
    }

    private object Test37 extends TestFunction[PathFinder[StepData]](37, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString37), isTalkative)
        }
    }

    private object Test38 extends TestFunction[PathFinder[StepData]](38, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString38), isTalkative)
        }
    }

    private object Test39 extends TestFunction[PathFinder[StepData]](39, false) {
        def apply(pathFinder: PathFinder[StepData], isTalkative: Boolean) : Boolean = {
            analyze(pathFinder(TestMapString39), isTalkative)
        }
    }

    // ==============================+--------------------------------------------+==================================
    // >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|   Code for test map strings starts here    |<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    // ==============================+--------------------------------------------+==================================

    object TestMapString1 extends PathingMapString("*_____________G", "akjshdkjashldjaksdhljakds")

    object TestMapString2 extends PathingMapString("_*asdf" +
                                                   "G_asdf", "asdf")
    
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

    object TestMapString7 extends PathingMapString("*DG", "\\|")

    object TestMapString8 extends PathingMapString("G_____________*", "\\|")

    object TestMapString9 extends PathingMapString("*|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "_|" +
                                                   "G", "\\|")

    object TestMapString10 extends PathingMapString("G|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "*", "\\|")

    object TestMapString11 extends PathingMapString("_______*______G", "\\|")

    object TestMapString12 extends PathingMapString("_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "*|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "_|" +
                                                    "G", "\\|")

    object TestMapString13 extends PathingMapString("*_____________G|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________", "\\|")

    object TestMapString14 extends PathingMapString("G_____________*|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________", "\\|")

    object TestMapString15 extends PathingMapString("_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "*_____________G", "\\|")

    object TestMapString16 extends PathingMapString("_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "G_____________*", "\\|")

    object TestMapString17 extends PathingMapString("_______________|" +
                                                    "_______________|" +
                                                    "*_____________G|" +
                                                    "_______________|" +
                                                    "_______________", "\\|")

    object TestMapString18 extends PathingMapString("*______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "G______________", "\\|")

    object TestMapString19 extends PathingMapString("G______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "*______________", "\\|")

    object TestMapString20 extends PathingMapString("______________*|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "______________G", "\\|")

    object TestMapString21 extends PathingMapString("______________G|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "______________*", "\\|")

    object TestMapString22 extends PathingMapString("_______*_______|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______G_______", "\\|")

    object TestMapString23 extends PathingMapString("_______G_______|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______*_______", "\\|")

    object TestMapString24 extends PathingMapString("______________G|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "*______________", "\\|")

    object TestMapString25 extends PathingMapString("G______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "______________*", "\\|")

    object TestMapString26 extends PathingMapString("G______________|" +
                                                    "_______________|" +
                                                    "_______*_______|" +
                                                    "_______________|" +
                                                    "_______________", "\\|")

    object TestMapString27 extends PathingMapString("GD_DD___D______|" +
                                                    "___DD__D__D_D__|" +
                                                    "_D______D______|" +
                                                    "____D__D_____D_|" +
                                                    "_D__D______D__*", "\\|")

    object TestMapString28 extends PathingMapString("______________G|" +
                                                    "_____________D_|" +
                                                    "_____________D_|" +
                                                    "_____________D_|" +
                                                    "_____________D*", "\\|")

    object TestMapString29 extends PathingMapString("G______________|" +
                                                    "_______________|" +
                                                    "_______________|" +
                                                    "DDDDDDDDDDDDDD_|" +
                                                    "*______________", "\\|")

    object TestMapString30 extends PathingMapString("______D________|" +
                                                    "______D________|" +
                                                    "______D*D______|" +
                                                    "______DDD______|" +
                                                    "G______________", "\\|")

    object TestMapString31 extends PathingMapString("_______________|" +
                                                    "______D_D______|" +
                                                    "______D*D______|" +
                                                    "______DDD______|" +
                                                    "G______________", "\\|")

    object TestMapString32 extends PathingMapString("________D______|" +
                                                    "______D_D______|" +
                                                    "______D*D______|" +
                                                    "______DDD______|" +
                                                    "G______________", "\\|")

    object TestMapString33 extends PathingMapString("______D________|" +
                                                    "______D________|" +
                                                    "______D*D______|" +
                                                    "______D_D______|" +
                                                    "G______________", "\\|")

    object TestMapString34 extends PathingMapString("______D________|" +
                                                    "______D________|" +
                                                    "_______*D______|" +
                                                    "______DDD______|" +
                                                    "G______________", "\\|")

    object TestMapString35 extends PathingMapString("_______________|" +
                                                    "______DDD______|" +
                                                    "______D*D______|" +
                                                    "______DDD______|" +
                                                    "G______________", "\\|")

    object TestMapString36 extends PathingMapString("______________________________________________|" +
                                                    "______________________________________________|" +
                                                    "______________________________________________|" +
                                                    "_________________DDDDDDDDDDDDD________________|" +
                                                    "_________________D____D__D___D________________|" +
                                                    "_________________D__D________D________________|" +
                                                    "_________________D_________D_D________________|" +
                                                    "_________________D_D_________D________________|" +
                                                    "_________________D____*______D________________|" +
                                                    "_________________D__________DD________________|" +
                                                    "_________________D___D_______D________________|" +
                                                    "_________________DD__D___D___D________________|" +
                                                    "_________________DDDDDDDDDDDDD________________|" +
                                                    "______________________________________________|" +
                                                    "_______G______________________________________|" +
                                                    "______________________________________________|" +
                                                    "______________________________________________", "\\|")

    object TestMapString37 extends PathingMapString("_______________|" +
                                                    "______DDD______|" +
                                                    "______DGD______|" +
                                                    "______DDD______|" +
                                                    "*______________", "\\|")

    object TestMapString38 extends PathingMapString("______________________________________________|" +
                                                    "______________________________________________|" +
                                                    "______________________________________________|" +
                                                    "_________________DDDDDDDDDDDDD________________|" +
                                                    "_________________D____D__D___D________________|" +
                                                    "_________________D__D________D________________|" +
                                                    "_________________D_________D_D________________|" +
                                                    "_________________D_D_________D________________|" +
                                                    "_________________D____G______D________________|" +
                                                    "_________________D__________DD________________|" +
                                                    "_________________D___D_______D________________|" +
                                                    "_________________DD__D___D___D________________|" +
                                                    "_________________DDDDDDDDDDDDD________________|" +
                                                    "______________________________________________|" +
                                                    "_______*______________________________________|" +
                                                    "______________________________________________|" +
                                                    "______________________________________________", "\\|")

    object TestMapString39 extends PathingMapString("______________________________________________|" +
                                                    "_*____________________________________________|" +
                                                    "________________DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD|" +
                                                    "________________DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD|" +
                                                    "________________DD____D__D____________________|" +
                                                    "________________DD__D____f___DDDDDDDDDDDDDDDD_|" +
                                                    "________________DD_______f_D_DD_____________D_|" +
                                                    "________________DD_D______fffDD____________D__|" +
                                                    "________________DD____G______DD_____________D_|" +
                                                    "________________DD___D______DDD_DDDDDDDDDDDDD_|" +
                                                    "________________DD___D___D___DD_DD____________|" +
                                                    "_DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD_DDDDDDDDDDDDD_|" +
                                                    "_DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD_D_D___D___D_|" +
                                                    "__________________________D___D___D_D_D_D_D_D_|" +
                                                    "DDDDDDDDDDDDDDDDDDDDDDDDD___D_____D___D___D___|" +
                                                    "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD|" +
                                                    "________________________DDDDDDDDDDDDDDDDDDDDDD", "\\|")

}