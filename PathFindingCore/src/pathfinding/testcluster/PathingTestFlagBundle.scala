package pathfinding.testcluster

import tester.testcluster.TestFuncFlagBundle

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 11:58 PM
 */

class PathingTestFlagBundle(flagList: List[Boolean]) extends TestFuncFlagBundle(flagList) {
    def getAnalysisFlags : List[Boolean] = {
        get
    }
}