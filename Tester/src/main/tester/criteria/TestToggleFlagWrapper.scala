package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/20/11
 * Time: 6:51 PM
 */

class TestToggleFlagWrapper(toggles: Seq[TestCriteriaToggleFlag])
                           (implicit passItOn: Seq[TestToggleFlag] = toggles map ( _.flag ))
                            extends BaseToggleFlagWrapper(passItOn, TestingFlag.flags collect { case x: TestToggleFlag => x })
