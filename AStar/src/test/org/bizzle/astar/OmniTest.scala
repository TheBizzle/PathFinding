package org.bizzle.astar

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/14/12
 * Time: 10:16 PM
 */

object OmniTest extends OmniTestBase {
  run()
  protected def tests = Seq((AStarDependencyTest, "dependency"), (AStarTest, "AStar"), (BiDirAStarTest, "BiDir"))
}

object OmniTestQuiet extends OmniTestBase {
  run()
  protected def tests = Seq((AStarDependencyTest, "dependency"), (AStarQuiet, "AStar"), (BiDirQuiet, "BiDir"))
}
