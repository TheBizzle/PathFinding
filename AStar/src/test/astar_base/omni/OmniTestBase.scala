package astar_base.omni

import pathfinding.testscript.TestScript

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
        println("Start %s test:\n".format(name))
        test.main(Array[String]())
        println("\nEnd %s test.".format(name))
    }
  }

}
