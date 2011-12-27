package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:37 PM
 */

class ContradictoryArgsException(s: String = "") extends Exception("Contradictory flags detected: " + s)