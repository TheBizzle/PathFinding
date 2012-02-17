package tester.testcluster.testfunction

import tester.criteria.{BaseToggleFlagWrapper, Talkative, TestToggleFlag}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 10:26 PM
 */

class TestFuncFlagBundle(inToggles: List[TestToggleFlag], expandedSupportList: List[TestToggleFlag] = List())
                        (implicit passItOn: List[TestToggleFlag] = expandedSupportList ::: List(Talkative))
                         extends BaseToggleFlagWrapper(inToggles, passItOn)
