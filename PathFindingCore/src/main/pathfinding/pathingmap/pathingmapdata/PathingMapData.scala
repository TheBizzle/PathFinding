package pathfinding.pathingmap.pathingmapdata

import pathfinding.coordinate.Coordinate2D
import pathfinding.pathingmap.terrain.Terrain

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 10:51 PM
 */

case class PathingMapData(start: Coordinate2D, goal: Coordinate2D, cols: Int, rows: Int, arr: Array[Array[Terrain]])
