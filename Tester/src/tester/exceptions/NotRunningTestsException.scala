package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:45 PM
 */

class NotRunningTestsException(s: String = "") extends Exception("No tests are being run: " + s)