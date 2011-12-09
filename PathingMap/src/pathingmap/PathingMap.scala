package pathingmap

import coordinate.Coordinate
import direction._
import pathingmapdata.{PathingMapStringInterpreter, PathingMapString}
import terrain._
import java.security.InvalidParameterException

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

    def getTerrain(coord: Coordinate) : Terrain = {

        import coord._

        if (((x >= 0) && (x < colCount)) && ((y >= 0) && (y < rowCount)))
            pathingMap(x)(y)
        else
            Invalid
        
    }

    def neighborsOf(loc: Coordinate) : List[Direction] = {
        PathingMap.DirList.foldLeft(List[Direction]()) ((acc,dir) => if (PathingMap.isPassable(getTerrain(PathingMap.findNeighborCoord(loc, dir)))) dir::acc else acc)
    }

    def step(start: Coordinate, end: Coordinate) {
        pathingMap(start.x)(start.y) = Query
        pathingMap(end.x)(end.y) = Self
    }

    override def toString : String = {

        var acc = List[String]()

        for (j <- (rowCount - 1 to 0 by -1)) {
            var temp = List('\n')
            for (i <- (colCount - 1 to 0 by -1)) {
                temp = PathingMap.terrainToChar(pathingMap(i)(j))::temp
            }
            acc = (temp.mkString)::acc  // Prepend to the accumulator the string form of row "j" of this PathingMap
        }

        acc.reverse.mkString

    }

    override def clone() : PathingMap = {
        new PathingMap(colCount, rowCount, pathingMap.clone())
    }

}

object PathingMap {

    val DirList = List(North, East, South, West)

    def apply(mapString: PathingMapString) : (Coordinate, Coordinate, PathingMap) = {
        val pathingData = PathingMapStringInterpreter(mapString);   import pathingData._
        (start, goal, new PathingMap(cols, rows, arr))
    }

    private def terrainToChar(terrain: Terrain) : Char = {
        TerrainCharConverter(terrain) match {
            case Some(x) => x
            case None => throw new InvalidParameterException("" + terrain)
        }
    }

    def isPassable(terrain: Terrain) : Boolean = {
        terrain match {
            case Food | Mound | Empty | Goal => true
            case _ => false
        }
    }

    def findNeighborCoord(loc: Coordinate, dir: Direction) : Coordinate = {
        dir match {
            case North => new Coordinate(loc.x, loc.y + 1)
            case South => new Coordinate(loc.x, loc.y - 1)
            case East  => new Coordinate(loc.x + 1, loc.y)
            case West  => new Coordinate(loc.x - 1, loc.y)
            case _ => throw new InvalidParameterException
        }
    }

    def findDirection(start: Coordinate, end: Coordinate) : Direction = {
        if      (end.y == start.y + 1) North
        else if (end.y == start.y - 1) South
        else if (end.x == start.x + 1) East
        else if (end.x == start.x - 1) West
        else throw new InvalidParameterException
    }

    def generateCloneWithPath(path: List[Coordinate], inMap: PathingMap) : PathingMap = {
        val outMap = new PathingMap(inMap.colCount, inMap.rowCount, inMap.pathingMap.clone())
        path.foreach { case(coord) => outMap.pathingMap(coord.x)(coord.y) = Path }
        outMap
    }

}

/*******************************************************************************************
 *  Code from before the serious refactoring done here, and for pathingMap(rows)(columns)  *
 *      [rather than pathingMap(columns)(rows), which is how it logically SHOULD be]       *
 *******************************************************************************************/

// For x(rows)(cols) (crap code, anyway)
//
//    def processToArr(s: String) : Array[Array[Terrain]] = {
//
//        val out = s.split("\\|") map (x => new StringBuilder(x).toArray) map (x => x map (x => x match {
//            case 'a' => Ant
//            case 'G' => Goal
//            case '_' => Empty
//            case 'f' => Food
//            case 'O' => Mound
//            case 'D' => Wall
//            case '^' => Water
//            case 'x' => Path
//            case '.' => Query
//            case '*' => Self
//            case _ => throw new InvalidParameterException("" + x)
//        }))
//
//        cols = out.length
//        rows = out(0).length
//        out
//
//    }

// For x(rows)(cols) (crap code)
//
//    override def toString : String = {
//        pathingmap map (w => (w.foldRight(List[Char]())((x,acc) => (x match {
//            case Ant => 'a'
//            case Goal => 'G'
//            case Empty => '_'
//            case Food => 'f'
//            case Mound => 'O'
//            case Wall => 'D'
//            case Water => '^'
//            case Path => 'x'
//            case Query => '.'
//            case Self => '*'
//            case _ => throw new InvalidParameterException("" + x)
//        })::acc)).mkString ) reduceLeft ((s1, s2) => s1 + "\n" + s2)
//    }


/*****************************
 *     No longer needed      *
 *****************************/
// Makes a transposed string... : /
//
//    override def toString : String = {
//        pathingMap.foldRight(List[String]())((x,accX) => {
//             x.foldRight(List[Char]('\n'))((y,accY) => {
//                 terrainToChar(y)::accY
//             }).mkString::accX
//        }).mkString
//    }

