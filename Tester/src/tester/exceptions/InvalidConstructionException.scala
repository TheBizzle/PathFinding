package tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/29/12
 * Time: 6:33 PM
 */

class InvalidConstructionException(s: String = "") extends Exception("Object was constructed improperly: " + s)