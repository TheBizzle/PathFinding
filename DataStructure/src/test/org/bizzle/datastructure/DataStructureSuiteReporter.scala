package org.bizzle.datastructure

import
  org.bizzle.tester.suite.SuiteReporter

import
  org.bizzle.datastructure.mutable._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/18/12
 * Time: 10:58 PM
 */

object DataStructureSuiteReporter extends SuiteReporter(Seq(new BiHashMapFunSuite()))
