package org.bizzle.pathfinding

import
  org.bizzle.tester.cluster.Testable

import
  pathingmap.PathingMapString

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/23/11
 * Time: 5:09 PM
 */

trait PathFinder[T <: StepData] extends Testable {
  def apply(mapString: PathingMapString) : PathingStatus[T]
}
