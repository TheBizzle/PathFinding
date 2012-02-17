package pathfinding.pathingmap.pathingmapdata

import tester.testcluster.TestSubject

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 9:43 PM
 */

/**
 * @param str        The string that represents the pathing map
 * @param delim      The line delimiter sequence for 'str'.  Must take on a regex-matchable form (the same as what String.split() takes)
 */
case class PathingMapString(str: String, delim: String) extends TestSubject
