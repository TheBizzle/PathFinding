package tester.testcluster.testfunction

import tester.criteria.{BaseToggleFlagWrapper, Talkative, TestToggleFlag}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 10:26 PM
 */

class TestFuncFlagBundle(inToggles: Seq[TestToggleFlag], extras: Seq[TestToggleFlag] = Seq())
                        (implicit passItOn: Seq[TestToggleFlag] = extras ++ Seq(Talkative))
                         extends BaseToggleFlagWrapper(inToggles, passItOn)
