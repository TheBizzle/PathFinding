package pathfinding.testcluster

import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.pathingmap.PathingMap
import tester.testcluster.TestCluster
import pathfinding.statuses.{Failure, Success, PathingStatus}
import pathfinding.StepData
import pathfinding.coordinate.{PriorityCoordinate, Coordinate}
import tester.testanalyzer.TestAnalyzer
import tester.exceptions.{InvalidConstructionException, MysteriousDataException}
import java.lang.reflect.Field

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 4:28 PM
 */

object PathingTestCluster extends TestCluster[PathingTestFunction, PathingMapString] with TestAnalyzer[PathingStatus[StepData], PathingAnalysisFlagBundle] {

    lazy val tests = generateTests

    def getTestsToRun(testNums: List[Int]) : List[PathingTestFunction] = {
        tests.zipWithIndex.filter { case(x, y) => testNums.contains(y) }.map ( _._1 ).toList
    }

    def getSize : Int = {
        tests.length - 1
    }

    protected def analyze(status: PathingStatus[StepData], flags: PathingAnalysisFlagBundle) : Boolean = {

        val (isTalkative) = flags.get match {
            case h :: Nil => (h)
            case _        => throw new InvalidConstructionException("Improperly-constructed flag bundle!")
        }

        status match {
            case Success(x) => {
                if (isTalkative) println("\n\nFound a solution!")
                retracePath(x.breadcrumbArr, x.endGoal, x.pathingMap, isTalkative)
                true
            }
            case Failure(_) => {
                if (isTalkative) println("\n\nFailed to find a solution....")
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
            println("So, anyway... you should move " + PathingMap.findDirection(pathTaken.head, suggestedLoc) + " towards " + suggestedLoc)

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

    protected def generateTestFunction(testField: (Field, String), regex: String) : Option[PathingTestFunction] = {
        if (testField._2.matches(regex)) {
            val Matcher = regex.r
            val Matcher(testNum, shouldPass) = testField
            val func = construct(testField._1.asInstanceOf[PathingMapString], testNum.toInt, (shouldPass == null))
            Some(func)
        }
        else None
    }

    protected def construct(subject: PathingMapString, testNumber: Int, shouldPass: Boolean) : PathingTestFunction = {
        new PathingTestFunction(subject, analyze, testNumber, shouldPass)
    }

    // \=============================/--------------------------------------------\=================================/
    // ------------------------------|   Code for test map strings starts here    |----------------------------------
    // /=============================\--------------------------------------------/=================================\
    // |                                                                                                            |
    // | Look... there's no pleasant way to handle this scenario.  I want to be able to nicely draw a map in ASCII. |
    // |   Before, I tied each map string into a TestFunction-extending singleton, but I also had to pass in the    |
    // |     desired pass/fail effect and the test number.  It ate up a lot of space and was pretty redundant.      |
    // |    I could wrap the PathingMapStrings and their metadata inside a list, effectively avoiding spacewaste    |
    // |  issues, but... then readability suffers, and I feel that readability is very important when it comes to   |
    // |   drawing ASCII maps.  So... I'm leaving the collection of PathingMapStrings here as it is.  I will use    |
    // |  reflection to scrape info from the name of each PathingMapString, which must take on the regex-matchable  |
    // |   form of "TestMapString([0-9]+)(F?)"  =>                                                                  |
    // |    $1: The test number (which will get passed as an argument to the TestFunction).                         |
    // |    $2: "F" or 'null'.  If the former, the TestFunction is told to expect the test on this string to fail.  |
    // |                        If the latter, the TestFunction is told nothing (implying that it should succeed).  |
    // |                                                                                                            |
    // \============================================================================================================/

    object TestMapString1 extends PathingMapString("*_____________G", "akjshdkjashldjaksdhljakds")

    object TestMapString2 extends PathingMapString("_*asdf" +
                                                   "G_asdf", "asdf")

    object TestMapString3F extends PathingMapString("_%__*|" +
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

    object TestMapString7F extends PathingMapString("*DG", "\\|")

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

    object TestMapString35F extends PathingMapString("_______________|" +
                                                     "______DDD______|" +
                                                     "______D*D______|" +
                                                     "______DDD______|" +
                                                     "G______________", "\\|")

    object TestMapString36F extends PathingMapString("______________________________________________|" +
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

    object TestMapString37F extends PathingMapString("_______________|" +
                                                     "______DDD______|" +
                                                     "______DGD______|" +
                                                     "______DDD______|" +
                                                     "*______________", "\\|")

    object TestMapString38F extends PathingMapString("______________________________________________|" +
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

    object TestMapString39F extends PathingMapString("______________________________________________|" +
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