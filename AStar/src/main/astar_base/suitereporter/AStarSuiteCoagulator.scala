package astar_base.suitereporter


import tester.{ suite, TesterSuiteReporter }, suite.SuiteCoagulator

import datastructure.DataStructureSuiteReporter
import pathfinding.PathFindingSuiteReporter

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 11:01 PM
 */

// Pass all of the SuiteReporters of your class—and of your dependencies—to the superclass's constructor!
object AStarSuiteCoagulator extends SuiteCoagulator(AStarSuiteReporter, DataStructureSuiteReporter, PathFindingSuiteReporter, TesterSuiteReporter)
