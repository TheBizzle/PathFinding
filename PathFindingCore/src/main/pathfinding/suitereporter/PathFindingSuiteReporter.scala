package pathfinding.suitereporter

import tester.suitereporter.SuiteReporter
import pathfinding.pathingmap.PathingMapSpec

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:57 PM
 */

object PathFindingSuiteReporter extends SuiteReporter(Seq(new PathingMapSpec()))
