package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:48 PM
 */

class UnexpectedTypeException(s: String = "") extends Exception("Unexpected type encountered: " + s)
