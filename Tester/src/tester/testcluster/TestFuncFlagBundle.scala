package tester.testcluster

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 10:26 PM
 */

//@ Should be using TestToggleFlags.  As should the AnalysisFlagBundle
abstract class TestFuncFlagBundle(flagList: List[Boolean]) {

    val flags = flagList

    def get : List[Boolean] = {
        flags
    }

    def getAnalysisFlags : List[Boolean]

}