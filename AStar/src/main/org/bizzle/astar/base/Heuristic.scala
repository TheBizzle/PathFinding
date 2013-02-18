package org.bizzle.astar.base

import
  org.bizzle.pathfinding.coordinate.Coordinate2D

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 1:38 PM
 */

object HeuristicLib {
  def manhattanDistance(h: HeuristicBundle) : Int = {
    (h.start.x - h.end.x).abs + (h.start.y - h.end.y).abs
  }
}

case class HeuristicBundle(start: Coordinate2D, end: Coordinate2D)
