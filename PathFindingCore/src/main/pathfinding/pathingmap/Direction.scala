package pathfinding.pathingmap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 1:41 AM
 */

sealed trait Direction {
  override def toString = this.getClass.getName.init
}

private[pathingmap] object Direction {
  object North extends Direction
  object East  extends Direction
  object South extends Direction
  object West  extends Direction
}
