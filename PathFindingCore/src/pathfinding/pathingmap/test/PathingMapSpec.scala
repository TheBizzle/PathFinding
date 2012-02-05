package pathfinding.pathingmap.test

import org.scalatest.{GivenWhenThen, FlatSpec}
import pathfinding.pathingmap.PathingMap
import pathfinding.pathingmap.pathingmapdata.PathingMapString
import pathfinding.coordinate.Coordinate
import pathfinding.pathingmap.terrain._
import pathfinding.pathingmap.direction._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/27/11
 * Time: 12:42 PM
 */

class PathingMapSpec extends FlatSpec with GivenWhenThen {

    val (start, goal, pMap) = PathingMap(new PathingMapString("_%__*|" +
                                                              "OG%_%|" +
                                                              "%____|", "\\|"))

    behavior of "A PathingMap"

    it should "correctly report the type of terrain at a given coordinate" in {

        given("a 4x3 pathing map with a goal at (1, 1), nothing at (1, 0)")
        when("asked for the terrain at those coordinates and the terrain at an out-of-bounds coordinate")
        val terrain1 = pMap.getTerrain(Coordinate(1, 1))
        val terrain2 = pMap.getTerrain(Coordinate(1, 0))
        val terrain3 = pMap.getTerrain(Coordinate(4, 3))

        then("the first should be the goal")
        terrain1 === Goal

        and("the second should be empty")
        terrain2 === Empty

        and("the third should be invalid")
        terrain3 === Invalid

    }

    it should "correctly report all passable neighbors of a given coordinate" in {

        given("a 4x3 pathing map with a goal at (1, 1), with only (0, 1) and (1, 0) as passable neighbors")
        when("asked for the neighbors of (1, 1) and the neighbors of (4, 3)")
        val goodTerrainList = pMap.neighborsOf(Coordinate(1, 1))
        val emptyTerrainList = pMap.neighborsOf(Coordinate(4, 3))

        then("the first should contain only (0, 1) and (1, 0)")
        ((goodTerrainList == List(Coordinate(0, 1), Coordinate(1, 0))) || (goodTerrainList == List(Coordinate(1, 0), Coordinate(0, 1)))) === true

        and("the second should be empty")
        emptyTerrainList === Nil

    }

    it should "correctly report what coordinate is to the <Direction> of a given coordinate" in {

        given("a coordinate and four different directions")
        val baseCoord = Coordinate(5, 5)
        val directions = Array(North, South, West, East)

        when("asked for the neighbor coordinate in each of those directions")

        then("it should report the first coordinate correctly")
        PathingMap.findNeighborCoord(baseCoord, directions(0)) === Coordinate(5, 6)

        and("the second")
        PathingMap.findNeighborCoord(baseCoord, directions(1)) === Coordinate(5, 4)

        and("the third")
        PathingMap.findNeighborCoord(baseCoord, directions(2)) === Coordinate(4, 5)

        and("the fourth")
        PathingMap.findNeighborCoord(baseCoord, directions(3)) === Coordinate(6, 5)

    }

    it should "correctly report what direction one coordinate is from another" in {

        given("a coordinate and four different, adjacent coordinates")
        val baseCoord = Coordinate(1, 4)
        val coords = Array(Coordinate(1, 3), Coordinate(0, 4), Coordinate(1, 5), Coordinate(2, 4))

        when("asked for the neighbor coordinate in each of those directions")

        then("it should report the first coordinate correctly")
        PathingMap.findDirection(baseCoord, coords(0)) === South

        and("the second")
        PathingMap.findDirection(baseCoord, coords(1)) === West

        and("the third")
        PathingMap.findDirection(baseCoord, coords(2)) === North

        and("the fourth")
        PathingMap.findDirection(baseCoord, coords(3)) === East

    }

}
