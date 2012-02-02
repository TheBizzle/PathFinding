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
        tests.zipWithIndex.filter { case(x, y) => testNums.contains(y + 1) }.map ( _._1 ).toList
    }

    def getSize : Int = {
        tests.length
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
            val Matcher(testNum, shouldPass) = testField._2
            val func = construct(testField._1.get(this).asInstanceOf[PathingMapString], testNum.toInt, (shouldPass == null))
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

    private val TestMapString1 = new PathingMapString("*_____________G", "akjshdkjashldjaksdhljakds")

    private val TestMapString2 = new PathingMapString("_*asdf" +
                                                      "G_asdf", "asdf")

    private val TestMapString3F = new PathingMapString("_%__*|" +
                                                       "OG%_%|" +
                                                       "%%___|", "\\|")

    private val TestMapString4 = new PathingMapString("_%__*|" +
                                                      "OG%_%|" +
                                                      "%____|", "\\|")

    private val TestMapString5 = new PathingMapString("_______________|" +
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

    private val TestMapString6 = new PathingMapString("_______________|" +
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

    private val TestMapString7F = new PathingMapString("*DG", "\\|")

    private val TestMapString8 = new PathingMapString("G_____________*", "\\|")

    private val TestMapString9 = new PathingMapString("*|" +
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

    private val TestMapString10 = new PathingMapString("G|" +
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

    private val TestMapString11 = new PathingMapString("_______*______G", "\\|")

    private val TestMapString12 = new PathingMapString("_|" +
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

    private val TestMapString13 = new PathingMapString("*_____________G|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________", "\\|")

    private val TestMapString14 = new PathingMapString("G_____________*|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________", "\\|")

    private val TestMapString15 = new PathingMapString("_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "*_____________G", "\\|")

    private val TestMapString16 = new PathingMapString("_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "G_____________*", "\\|")

    private val TestMapString17 = new PathingMapString("_______________|" +
                                                       "_______________|" +
                                                       "*_____________G|" +
                                                       "_______________|" +
                                                       "_______________", "\\|")

    private val TestMapString18 = new PathingMapString("*______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "G______________", "\\|")

    private val TestMapString19 = new PathingMapString("G______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "*______________", "\\|")

    private val TestMapString20 = new PathingMapString("______________*|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "______________G", "\\|")

    private val TestMapString21 = new PathingMapString("______________G|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "______________*", "\\|")

    private val TestMapString22 = new PathingMapString("_______*_______|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______G_______", "\\|")

    private val TestMapString23 = new PathingMapString("_______G_______|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______*_______", "\\|")

    private val TestMapString24 = new PathingMapString("______________G|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "*______________", "\\|")

    private val TestMapString25 = new PathingMapString("G______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "______________*", "\\|")

    private val TestMapString26 = new PathingMapString("G______________|" +
                                                       "_______________|" +
                                                       "_______*_______|" +
                                                       "_______________|" +
                                                       "_______________", "\\|")

    private val TestMapString27 = new PathingMapString("GD_DD___D______|" +
                                                       "___DD__D__D_D__|" +
                                                       "_D______D______|" +
                                                       "____D__D_____D_|" +
                                                       "_D__D______D__*", "\\|")

    private val TestMapString28 = new PathingMapString("______________G|" +
                                                       "_____________D_|" +
                                                       "_____________D_|" +
                                                       "_____________D_|" +
                                                       "_____________D*", "\\|")

    private val TestMapString29 = new PathingMapString("G______________|" +
                                                       "_______________|" +
                                                       "_______________|" +
                                                       "DDDDDDDDDDDDDD_|" +
                                                       "*______________", "\\|")

    private val TestMapString30 = new PathingMapString("______D________|" +
                                                       "______D________|" +
                                                       "______D*D______|" +
                                                       "______DDD______|" +
                                                       "G______________", "\\|")

    private val TestMapString31 = new PathingMapString("_______________|" +
                                                       "______D_D______|" +
                                                       "______D*D______|" +
                                                       "______DDD______|" +
                                                       "G______________", "\\|")

    private val TestMapString32 = new PathingMapString("________D______|" +
                                                       "______D_D______|" +
                                                       "______D*D______|" +
                                                       "______DDD______|" +
                                                       "G______________", "\\|")

    private val TestMapString33 = new PathingMapString("______D________|" +
                                                       "______D________|" +
                                                       "______D*D______|" +
                                                       "______D_D______|" +
                                                       "G______________", "\\|")

    private val TestMapString34 = new PathingMapString("______D________|" +
                                                       "______D________|" +
                                                       "_______*D______|" +
                                                       "______DDD______|" +
                                                       "G______________", "\\|")

    private val TestMapString35F = new PathingMapString("_______________|" +
                                                        "______DDD______|" +
                                                        "______D*D______|" +
                                                        "______DDD______|" +
                                                        "G______________", "\\|")

    private val TestMapString36F = new PathingMapString("______________________________________________|" +
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

    private val TestMapString37F = new PathingMapString("_______________|" +
                                                        "______DDD______|" +
                                                        "______DGD______|" +
                                                        "______DDD______|" +
                                                        "*______________", "\\|")

    private val TestMapString38F = new PathingMapString("______________________________________________|" +
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

    private val TestMapString39F = new PathingMapString("______________________________________________|" +
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