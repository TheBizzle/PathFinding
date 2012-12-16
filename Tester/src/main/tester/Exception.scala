package tester

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/16/12
 * Time: 10:23 AM
 */

class ContradictoryArgsException(s: String = "") extends Exception("Contradictory flags detected: " + s)
class InvalidTestNumberException(s: String = "") extends Exception("Invalid test number referenced: " + s)
class MysteriousDataException   (s: String = "") extends Exception("Rather mysterious data detected: " + s)
class NotRunningTestsException  (s: String = "") extends Exception("No tests are being run: " + s)
class RedundancyException       (s: String = "") extends Exception("Redundant inclusion: " + s)

