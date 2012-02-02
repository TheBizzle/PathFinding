package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/20/11
 * Time: 6:51 PM
 */

class TestToggleFlagWrapper(toggles: List[TestCriteriaToggleFlag])
                           (implicit passItOn: List[TestToggleFlag] = toggles map ( _.criteria ))
                            extends BaseToggleFlagWrapper(passItOn, List(Talkative, RunBaseTests, SkipPathingTests, StackTrace))