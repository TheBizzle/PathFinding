package main.org.bizzle

import org.bizzle.pathfinding.coordinate.{ Coordinate2D, PriorityCoordinate }

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 3/3/13
 * Time: 9:47 AM
 */
package object astar {
  protected type PFCoord = Coordinate2D with PriorityCoordinate
}
