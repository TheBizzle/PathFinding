import astar.AStar
import astar.test.mapstring._
import astar_base.tester.PathingTestAnalyzer

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/3/11
 * Time: 11:08 PM
 */

// At some point, turn this stuff into real tests using ScalaTest.
// Refactor it into AStarBase, and also create individual tests for the different things that extend AStarBase
PathingTestAnalyzer(AStar(TestMapString))
PathingTestAnalyzer(AStar(TestMapString1))
PathingTestAnalyzer(AStar(TestMapString2))
PathingTestAnalyzer(AStar(TestMapString3))
PathingTestAnalyzer(AStar(TestMapString4))
PathingTestAnalyzer(AStar(TestMapString5))