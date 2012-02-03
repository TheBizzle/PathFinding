package pathfinding.pathingmap.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/27/11
 * Time: 12:55 PM
 */

class InvalidParameterException(s: String = "") extends Exception("One or more passed-in parameters was invalid: " + s)
