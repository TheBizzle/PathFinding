package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 8:17 PM
 */

class MysteriousDataException(s: String = "") extends Exception("Rather mysterious data detected: " + s)