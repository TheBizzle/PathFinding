package pathfinding.pathingmap.pathingmapdata

import pathfinding.pathingmap.terrain._
import pathfinding.coordinate.Coordinate
import pathfinding.pathingmap.exceptions.{InvalidMapStringException, InvalidParameterException}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 10:50 PM
 */

object PathingMapStringInterpreter {

    def apply(mapString: PathingMapString) : PathingMapData = {

        val splitArr = mapString.str.split(mapString.delim)
        val rows = splitArr.length
        val cols = splitArr(0).length
        val outArr = convertArray(cols, rows, splitArr)
        val (start, goal) = findStartAndGoal(outArr, cols, rows)

        new PathingMapData(start, goal, cols, rows, outArr)

    }

    /**
     * Converts an array of strings into a 2D array of Terrains
     * Transposes the array (so that the old x becomes the new y and the old y becomes the new x)
     * Also vertically flips the array (so that y=0 is at the bottom, rather than the top)
     */
    private def convertArray(cols: Int, rows: Int, inArr: Array[String]) :  Array[Array[Terrain]] = {

        val outArr = PathingMapStringInterpreter.initialize2DTerrainArr(cols, rows, Invalid)

        for (y <- (0 until rows)) {

            val tempArr = new StringBuilder(inArr(y)).toArray   // Makes an indexable array of characters
            val realY = rows - 1 - y                            // Used for the vertical flip

            for (x <- (0 until cols)) {
                outArr(x)(realY) = charToTerrain(tempArr(x))
            }

        }

        outArr

    }

    private def findStartAndGoal(arr: Array[Array[Terrain]], cols: Int, rows: Int) : (Coordinate, Coordinate) = {

        var selfList = List[Coordinate]()
        var goalList = List[Coordinate]()

        for (x <- (0 until cols)) {
            for (y <- (0 until rows)) {
                if (arr(x)(y) == Self)
                    selfList = new Coordinate(x, y) :: selfList
                else if (arr(x)(y) == Goal)
                    goalList = new Coordinate(x, y) :: goalList
            }
        }

        if (selfList.size != 1)
            throw new InvalidMapStringException("Invalid number of starts!")

        if (goalList.size != 1)
            throw new InvalidMapStringException("Invalid number of goals!")

        (selfList.head, goalList.head)

    }

    private def charToTerrain(c: Char) : Terrain = {
        TerrainCharConverter(c) match {
            case Some(x) => x
            case None => throw new InvalidParameterException("" + c)
        }
    }

    private def initialize2DTerrainArr(cols: Int, rows: Int, defaultVal: Terrain) : Array[Array[Terrain]] = {
        new Array[Array[Terrain]](cols) map { case x => new Array[Terrain](rows) map {case y => defaultVal} }
    }

}
