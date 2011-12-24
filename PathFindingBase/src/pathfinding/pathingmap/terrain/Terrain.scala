package pathfinding.pathingmap.terrain

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 12:22 AM
 */

sealed abstract class Terrain

object Ant extends Terrain
object Empty extends Terrain
object Food extends Terrain
object Goal extends Terrain
object Invalid extends Terrain
object Mound extends Terrain
object Path extends Terrain
object Query extends Terrain
object Self extends Terrain
object Wall extends Terrain
object Water extends Terrain