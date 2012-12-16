package pathfinding.pathingmap

import collection.mutable.ListBuffer

import pathfinding.coordinate.{ Coordinate, Coordinate2D }
import Direction._
import Terrain._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 12:26 AM
 */

class PathingMap private (cols: Int, rows: Int, inArr: Array[Array[Terrain]]) {

  val colCount = cols
  val rowCount = rows
  private val pathingMap = inArr

  def getTerrain(coord: Coordinate2D) : Terrain = {

    import coord._

    if (((x >= 0) && (x < colCount)) && ((y >= 0) && (y < rowCount)))
      pathingMap(x)(y)
    else
      Invalid

  }

  def neighborsOf(loc: Coordinate2D) : Seq[Direction] = {
    PathingMap.Directions filter (x => getTerrain(PathingMap.findNeighborCoord(loc, x)).isPassable)
  }

  def step(start: Coordinate2D, end: Coordinate2D) {
    pathingMap(start.x)(start.y) = Query
    pathingMap(end.x)(end.y) = Self
  }

  def markAsGoal(coordinate: Coordinate2D) {
    pathingMap(coordinate.x)(coordinate.y) = Goal
  }

  override def toString : String = {

    val acc = new ListBuffer[String]()

    for (j <- (rowCount - 1 to 0 by -1)) {
      val buffer = new ListBuffer[Char]()
      pathingMap foreach (arr => buffer += PathingMap.terrainToChar(arr(j)))
      buffer += '\n'
      acc += buffer.mkString  // Append to the accumulator the string form of row "j" of this PathingMap
    }

    acc.mkString

  }

  override def clone() : PathingMap = {
    new PathingMap(colCount, rowCount, pathingMap map ( _.clone() ))
  }

}

object PathingMap {

  val Directions = Seq(North, East, South, West)

  def apply(mapString: PathingMapString) : (Coordinate2D, Coordinate2D, PathingMap) = {
    val pathingData = PathingMapStringInterpreter(mapString); import pathingData._
    (start, goal, new PathingMap(cols, rows, arr))
  }

  private def terrainToChar(terrain: Terrain) : Char = {
    Terrain(terrain).get
  }

  def findNeighborCoord(loc: Coordinate2D, dir: Direction) : Coordinate2D = {
    dir match {
      case North => Coordinate(loc.x, loc.y + 1)
      case South => Coordinate(loc.x, loc.y - 1)
      case East  => Coordinate(loc.x + 1, loc.y)
      case West  => Coordinate(loc.x - 1, loc.y)
      case _     => throw new UnknownSubclassException("" + dir)
    }
  }

  def findDirection(start: Coordinate2D, end: Coordinate2D) : Direction = {
    if      (end.y == start.y + 1) North
    else if (end.y == start.y - 1) South
    else if (end.x == start.x + 1) East
    else if (end.x == start.x - 1) West
    else throw new InvalidParameterException(start.toString + " or " + end.toString + " is/are invalid")
  }

  def generateCloneWithPath(path: Seq[Coordinate2D], inMap: PathingMap) : PathingMap = {
    val outMap = inMap.clone()
    path foreach (coord => outMap.pathingMap(coord.x)(coord.y) = Path)
    outMap
  }

}

case class PathingMapData(start: Coordinate2D, goal: Coordinate2D, cols: Int, rows: Int, arr: Array[Array[Terrain]])
case class PathingMapString(str: String, delim: String) extends tester.cluster.TestSubject
