package pathfinding.testanalyzer

import pathfinding.coordinate.Coordinate2D
import tester.cluster.TestAnalysisResultBundle

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/5/12
 * Time: 5:25 PM
 */

class PathingAnalysisResultBundle(val wasSuccess: Boolean, val path: Seq[Coordinate2D]) extends TestAnalysisResultBundle
