package datastructure.test.suitereporter

import tester.test.suitereporter.SuiteReporter
import datastructure.heap.test.HeapSpec
import datastructure.bihashmap.test.BiHashMapSpec

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:58 PM
 */

object DataStructureSuiteReporter extends SuiteReporter(List(new HeapSpec(), new BiHashMapSpec()))
