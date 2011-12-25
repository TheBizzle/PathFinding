package pathfinding.tester.exceptions

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/25/11
 * Time: 4:52 PM
 */

class UnnecessaryExclusionException(s: String = "") extends Exception("Unnecessary exclusion: " + s)