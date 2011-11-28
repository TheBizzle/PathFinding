package com.AStar.coordinate

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 12:25 AM
 */

class Coordinate(xLoc: Int, yLoc: Int) {

    val x = xLoc
    val y = yLoc

    override def toString : String = {
        "(" + x + "," + y + ")"
    }

}