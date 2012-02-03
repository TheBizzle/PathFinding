package pathfinding.pathingmap.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/9/12
 * Time: 12:20 AM
 */

class InvalidMapStringException(s: String = "") extends Exception("The passed-in map string was indecipherable: " + s)
