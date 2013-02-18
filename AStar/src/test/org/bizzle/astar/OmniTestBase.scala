package org.bizzle.astar

import
  org.bizzle.pathfinding.TestScript

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/15/12
 * Time: 11:43 PM
 */

abstract class OmniTestBase extends TestScript {

  protected def tests: Seq[(TestScript, String)]

  def run() {
    tests foreach {
      case (test, name) =>
        println(s"Start $name test:\n")
        test.main(Array[String]())
        println(s"\nEnd $name test.")
    }
  }

}

