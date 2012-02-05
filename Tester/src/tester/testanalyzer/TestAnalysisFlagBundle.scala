package tester.testanalyzer

import tester.criteria.{BaseToggleFlagWrapper, Talkative, TestToggleFlag}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/29/12
 * Time: 6:26 PM
 */

// I might spice that class up a bit more later.  I wanted it returning tuples, but found it too cumbersome....
abstract class TestAnalysisFlagBundle(inToggles: List[TestToggleFlag], listExpansion: List[TestToggleFlag] = List())
                                     (implicit passItOn: List[TestToggleFlag] = listExpansion ::: List(Talkative))
                                      extends BaseToggleFlagWrapper(inToggles, passItOn)
