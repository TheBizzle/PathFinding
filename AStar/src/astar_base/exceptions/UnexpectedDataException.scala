package astar_base.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/9/12
 * Time: 12:24 AM
 */

class UnexpectedDataException(s: String = "") extends Exception("Unhandlable data encountered: " + s)