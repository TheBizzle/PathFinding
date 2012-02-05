package pathfinding.testanalyzer

import tester.testanalyzer.TestAnalysisResultBundle
import pathfinding.coordinate.Coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/5/12
 * Time: 5:25 PM
 */

class PathingAnalysisResultBundle(val wasSuccess: Boolean, val path: List[Coordinate]) extends TestAnalysisResultBundle
