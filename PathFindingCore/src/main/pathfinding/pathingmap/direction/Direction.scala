package pathfinding.pathingmap.direction

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 1:41 AM
 */

sealed abstract class Direction {
    override def toString : String = this.getClass.getName.dropRight(1)
}

object North extends Direction
object East extends Direction
object South extends Direction
object West extends Direction
