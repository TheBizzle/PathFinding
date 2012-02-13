package pathfinding.pathingmap.pathingmapdata

import tester.testcluster.TestSubject

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 9:43 PM
 */

/**
 * @param s          The string that represents the pathing map
 * @param lineDelim  The line delimiter sequence for s.  Must take on a regex-matchable form (the same as what String.split() takes)
 */
class PathingMapString(private val s: String, private val lineDelim: String) extends TestSubject {
    def str = s
    def delim = lineDelim
}
