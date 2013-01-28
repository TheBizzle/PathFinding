package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/2/12
 * Time: 12:45 AM
 */

//@ Looking back at it, I truly don't get this thing
class ToggleFlagManager(val toggles: Set[TestToggleFlag], supportedToggles: Set[TestToggleFlag]) {

  // If this triggers, you likely created a new flag and forgot to add it to the implementing class's `flags`
  require(toggles.subsetOf(supportedToggles))

  def contains(flag: TestToggleFlag) = toggles.contains(flag)

}

class TestToggleFlagManager(toggles: Set[TestCriteriaToggleFlag])
//  extends ToggleFlagManager(toggles map (_.flag), TestingFlag.flags collect { case x: TestToggleFlag => x })
  extends ToggleFlagManager(toggles map (_.flag), TestingFlag.flags filter { case x => x.isInstanceOf[TestToggleFlag] }) //@ Temporary workaround for above code failing on 2.10
