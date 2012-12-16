package datastructure.suitereporter

import tester.suite.SuiteReporter

import datastructure.mutable._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:58 PM
 */

object DataStructureSuiteReporter extends SuiteReporter(Seq(new BiHashMapFunSuite()))
