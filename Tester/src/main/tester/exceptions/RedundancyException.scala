package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:54 PM
 */

class RedundancyException(s: String = "") extends Exception("Redundant inclusion: " + s)
