package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:46 PM
 */

class InvalidTestNumberException(s: String = "") extends Exception("Invalid test number referenced: " + s)
