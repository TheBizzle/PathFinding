package tester.testanalyzer

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/29/12
 * Time: 6:26 PM
 */

// I might spice that class up a bit more later.  I wanted it returning tuples, but found it too cumbersome....
//@ Should probably receive a list of TestToggleFlags and search through it for the ones relevant to it.  Hold onto those ones.
abstract class TestAnalysisFlagBundle(inFlags: List[Boolean]) {
    private val flagList = inFlags
    def get : List[Boolean] = flagList
}