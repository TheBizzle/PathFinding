package datastructure.suitereporter

import tester.suitereporter.SuiteReporter
import datastructure.mutable.BiHashMapSpec

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:58 PM
 */

object DataStructureSuiteReporter extends SuiteReporter(Seq(new BiHashMapSpec()))
