package datastructure.suitereporter

import tester.suitereporter.SuiteReporter
import datastructure.heap.HeapSpec
import datastructure.bihashmap.BiHashMapSpec

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:58 PM
 */

object DataStructureSuiteReporter extends SuiteReporter(List(new HeapSpec(), new BiHashMapSpec()))
