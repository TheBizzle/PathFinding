package com.AStar.pathingmap.pathingmapdata

import com.AStar.pathingmap.terrain.Terrain
import com.AStar.coordinate.Coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 10:51 PM
 */

class PathingMapData(startCoord: Coordinate, goalCoord: Coordinate, colCount: Int, rowCount: Int, terrainArr: Array[Array[Terrain]]) {
    val start = startCoord
    val goal = goalCoord
    val cols = colCount
    val rows = rowCount
    val arr = terrainArr
}