package pathfinding.pathingmap.direction

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 1:41 AM
 */

sealed abstract class Direction

object North extends Direction {
    override def toString : String = "North"
}

object East extends Direction {
    override def toString : String = "East"
}

object South extends Direction {
    override def toString : String = "South"
}

object West extends Direction {
    override def toString : String = "West"
}