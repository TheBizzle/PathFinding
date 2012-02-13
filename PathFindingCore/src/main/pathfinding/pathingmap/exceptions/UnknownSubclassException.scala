package pathfinding.pathingmap.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/27/11
 * Time: 12:55 PM
 */

class UnknownSubclassException(s: String = "") extends Exception("Unknown subclass found: " + s)
