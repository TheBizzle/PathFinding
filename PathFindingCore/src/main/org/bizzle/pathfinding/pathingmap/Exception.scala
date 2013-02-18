package org.bizzle.pathfinding.pathingmap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/16/12
 * Time: 12:04 PM
 */

class InvalidMapStringException(s: String = "") extends Exception("The passed-in map string was indecipherable: " + s)
class InvalidParameterException(s: String = "") extends Exception("One or more passed-in parameters was invalid: " + s)
class UnknownSubclassException (s: String = "") extends Exception("Unknown subclass found: " + s)

