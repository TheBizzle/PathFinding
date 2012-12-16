package pathfinding

import java.lang.reflect.Field

import tester.{ cluster, criteria, MysteriousDataException }
import criteria.{ Talkative, TestToggleFlag }
import cluster.{ TestAnalysisFlagBundle, TestAnalysisResultBundle, TestAnalyzer, TestCluster }

import coordinate.Coordinate2D
import pathingmap.{ PathingMap, PathingMapString }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 4:28 PM
 */

class PathingAnalysisFlagBundle(inToggles: Seq[TestToggleFlag]) extends TestAnalysisFlagBundle(inToggles)
class PathingAnalysisResultBundle(val wasSuccess: Boolean, val path: Seq[Coordinate2D]) extends TestAnalysisResultBundle

object PathingTestCluster
  extends TestCluster[PathingTestFunction, PathingMapString, PTFConstructionBundle]
    with TestAnalyzer[PathingStatus[StepData], PathingAnalysisFlagBundle, PathingAnalysisResultBundle] {

  override protected def TestFunctionRegex = "TestMapString([0-9]+)((_L([0-9]+))?)"

  lazy val tests = generateTests

  override def getTestsToRun(testNums: Seq[Int]) : Seq[PathingTestFunction] = tests.zipWithIndex collect { case (x, y) if (testNums.contains(y + 1)) => x }
  override def getSize = tests.length

  override protected def analyze(status: PathingStatus[StepData], flags: PathingAnalysisFlagBundle) : PathingAnalysisResultBundle = {

    import PathingStatus._

    val isTalkative = flags.get(Talkative)
    val (wasSuccessful, path) = status match {
      case Success(x) =>
        if (isTalkative) println("\n\nFound a solution!")
        (true, retracePath(x.breadcrumbArr, x.endGoal, x.pathingMap, isTalkative))
      case Failure(_) =>
        if (isTalkative) println("\n\nFailed to find a solution....")
        (false, Seq())
      case x          =>
        throw new MysteriousDataException("Unexpected ExecutionStatus!  Received: " + x)
    }

    new PathingAnalysisResultBundle(wasSuccessful, path)

  }

  private def retracePath(breadcrumbs: Array[Array[Coordinate2D]], goal: Coordinate2D, pathingMap: PathingMap, isTalkative: Boolean) : Seq[Coordinate2D] = {

    val pathTaken = eatBreadcrumbsForPath(breadcrumbs, goal)

    if (isTalkative)
      println("The path taken was: " + pathTaken + "\nHere, let me draw that for you on the map!\n")

    pathingMap.markAsGoal(goal)

    if (isTalkative)
      println(PathingMap.generateCloneWithPath(pathTaken, pathingMap).toString)

    val suggestedLoc = pathTaken.tail.head

    if (isTalkative)
      println("So, anyway... you should move " + PathingMap.findDirection(pathTaken.head, suggestedLoc) + " towards " + suggestedLoc)

    pathTaken

  }

  private def eatBreadcrumbsForPath(breadcrumbs: Array[Array[Coordinate2D]], goal: Coordinate2D) : Seq[Coordinate2D] = {
    def breadcrumbsHelper(breadcrumbs: Array[Array[Coordinate2D]], current: Coordinate2D) : Seq[Coordinate2D] = {
      current +: {
        val next = breadcrumbs(current.x)(current.y)
        if (next.isValid) breadcrumbsHelper(breadcrumbs, next) else Seq()
      }
    }
    (goal +: breadcrumbsHelper(breadcrumbs, breadcrumbs(goal.x)(goal.y))).reverse
  }

  override protected def generateTestFunction(testField: (Field, String), regex: String) : Option[PathingTestFunction] = {
    if (testField._2.matches(regex)) {
      val Matcher = regex.r
      val Matcher(testNum, shouldPass, _, foundLength) = testField._2
      val expectedPathLength = if (foundLength != null) foundLength.toInt else -1
      val func = construct(testField._1.get(this).asInstanceOf[PathingMapString], testNum.toInt, !(shouldPass isEmpty), new PTFConstructionBundle(expectedPathLength))
      Option(func)
    }
    else None
  }

  override protected def construct(subject: PathingMapString, testNumber: Int, shouldPass: Boolean, bundle: PTFConstructionBundle) : PathingTestFunction = {
    new PathingTestFunction(subject, analyze, testNumber, shouldPass, bundle.expectedPathLength)
  }

  /*

  \=============================/--------------------------------------------\=================================/
  ------------------------------|   Code for test map strings starts here    |----------------------------------
  /=============================\--------------------------------------------/=================================\
  |                                                                                                            |
  | Look... there's no pleasant way to handle this scenario.  I want to be able to nicely draw a map in ASCII. |
  |   Before, I tied each map string into a TestFunction-extending singleton, but I also had to pass in the    |
  |     desired pass/fail effect and the test number.  It ate up a lot of space and was pretty redundant.      |
  |    I could wrap the PathingMapStrings and their metadata inside a list, effectively avoiding spacewaste    |
  |  issues, but... then readability suffers, and I feel that readability is very important when it comes to   |
  |   drawing ASCII maps.  So... I'm leaving the collection of PathingMapStrings here as it is.  I will use    |
  |  reflection to scrape info from the name of each PathingMapString, which must take on the regex-matchable  |
  |   form of "TestMapString([0-9]+)((_L([0-9]+))?)"  =>                                                       |
  |    $1: The test number (which will get passed as an argument to the TestFunction).                         |
  |    $2: The length of path that the tester should find.  If unspecified, the test will be expected to fail  |
  |                                                                                                            |
  \============================================================================================================/

  */

  private val TestMapString1_L14 = new PathingMapString("*_____________G", "akjshdkjashldjaksdhljakds")

  private val TestMapString2_L2 = new PathingMapString("_*asdf" +
                                                       "G_asdf", "asdf")

  private val TestMapString3 = new PathingMapString("_%__*|" +
                                                    "OG%_%|" +
                                                    "%%___|", "\\|")

  private val TestMapString4_L6 = new PathingMapString("_%__*|" +
                                                       "OG%_%|" +
                                                       "%____|", "\\|")

  private val TestMapString5_L39 = new PathingMapString("_______________|" +
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

  private val TestMapString6_L61 = new PathingMapString("_______________|" +
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

  private val TestMapString7 = new PathingMapString("*DG", "\\|")

  private val TestMapString8_L14 = new PathingMapString("G_____________*", "\\|")

  private val TestMapString9_L14 = new PathingMapString("*|" +
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

  private val TestMapString10_L14 = new PathingMapString("G|" +
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

  private val TestMapString11_L7 = new PathingMapString("_______*______G", "\\|")

  private val TestMapString12_L8 = new PathingMapString("_|" +
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

  private val TestMapString13_L14 = new PathingMapString("*_____________G|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________", "\\|")

  private val TestMapString14_L14 = new PathingMapString("G_____________*|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________", "\\|")

  private val TestMapString15_L14 = new PathingMapString("_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "*_____________G", "\\|")

  private val TestMapString16_L14 = new PathingMapString("_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "G_____________*", "\\|")

  private val TestMapString17_L14 = new PathingMapString("_______________|" +
                                                         "_______________|" +
                                                         "*_____________G|" +
                                                         "_______________|" +
                                                         "_______________", "\\|")

  private val TestMapString18_L4 = new PathingMapString("*______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "G______________", "\\|")

  private val TestMapString19_L4 = new PathingMapString("G______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "*______________", "\\|")

  private val TestMapString20_L4 = new PathingMapString("______________*|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "______________G", "\\|")

  private val TestMapString21_L4 = new PathingMapString("______________G|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "______________*", "\\|")

  private val TestMapString22_L4 = new PathingMapString("_______*_______|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______G_______", "\\|")

  private val TestMapString23_L4 = new PathingMapString("_______G_______|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______________|" +
                                                        "_______*_______", "\\|")

  private val TestMapString24_L18 = new PathingMapString("______________G|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "*______________", "\\|")

  private val TestMapString25_L18 = new PathingMapString("G______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "______________*", "\\|")

  private val TestMapString26_L9 = new PathingMapString("G______________|" +
                                                        "_______________|" +
                                                        "_______*_______|" +
                                                        "_______________|" +
                                                        "_______________", "\\|")

  private val TestMapString27_L20 = new PathingMapString("GD_DD___D______|" +
                                                         "___DD__D__D_D__|" +
                                                         "_D______D______|" +
                                                         "____D__D_____D_|" +
                                                         "_D__D______D__*", "\\|")

  private val TestMapString28_L4 = new PathingMapString("______________G|" +
                                                        "_____________D_|" +
                                                        "_____________D_|" +
                                                        "_____________D_|" +
                                                        "_____________D*", "\\|")

  private val TestMapString29_L32 = new PathingMapString("G______________|" +
                                                         "_______________|" +
                                                         "_______________|" +
                                                         "DDDDDDDDDDDDDD_|" +
                                                         "*______________", "\\|")

  private val TestMapString30_L15 = new PathingMapString("______D________|" +
                                                         "______D________|" +
                                                         "______D*D______|" +
                                                         "______DDD______|" +
                                                         "G______________", "\\|")

  private val TestMapString31_L13 = new PathingMapString("_______________|" +
                                                         "______D_D______|" +
                                                         "______D*D______|" +
                                                         "______DDD______|" +
                                                         "G______________", "\\|")

  private val TestMapString32_L13 = new PathingMapString("________D______|" +
                                                         "______D_D______|" +
                                                         "______D*D______|" +
                                                         "______DDD______|" +
                                                         "G______________", "\\|")

  private val TestMapString33_L9 = new PathingMapString("______D________|" +
                                                        "______D________|" +
                                                        "______D*D______|" +
                                                        "______D_D______|" +
                                                        "G______________", "\\|")

  private val TestMapString34_L9 = new PathingMapString("______D________|" +
                                                        "______D________|" +
                                                        "_______*D______|" +
                                                        "______DDD______|" +
                                                        "G______________", "\\|")

  private val TestMapString35 = new PathingMapString("_______________|" +
                                                     "______DDD______|" +
                                                     "______D*D______|" +
                                                     "______DDD______|" +
                                                     "G______________", "\\|")

  private val TestMapString36 = new PathingMapString("______________________________________________|" +
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

  private val TestMapString37 = new PathingMapString("_______________|" +
                                                     "______DDD______|" +
                                                     "______DGD______|" +
                                                     "______DDD______|" +
                                                     "*______________", "\\|")

  private val TestMapString38 = new PathingMapString("______________________________________________|" +
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

  private val TestMapString39 = new PathingMapString("______________________________________________|" +
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

