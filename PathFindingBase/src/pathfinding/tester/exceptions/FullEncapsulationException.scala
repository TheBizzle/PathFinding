package pathfinding.tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:49 PM
 */

class FullEncapsulationException(s: String = "") extends Exception("Full encapsulation encountered: " + s)