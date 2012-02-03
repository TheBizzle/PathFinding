package astar_base.test.suitereporter

import datastructure.test.suitereporter.DataStructureSuiteReporter
import pathfinding.test.suitereporter.PathFindingSuiteReporter
import tester.test.suitereporter.{TesterSuiteReporter, SuiteCoagulator}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:01 PM
 */

// Pass all of the SuiteReporters of your class—and of your dependencies—to the superclass's constructor!
object AStarSuiteCoagulator extends SuiteCoagulator(AStarSuiteReporter, DataStructureSuiteReporter, PathFindingSuiteReporter, TesterSuiteReporter)
