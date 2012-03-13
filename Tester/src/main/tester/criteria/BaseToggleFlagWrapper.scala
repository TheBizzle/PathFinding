package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/2/12
 * Time: 12:45 AM
 */

class BaseToggleFlagWrapper(toggles: List[TestToggleFlag], supportedToggles: List[TestToggleFlag]) {

  private val flagList = supportedToggles
  require(!toggles.map(flagList.contains(_)).contains(false))     // FlagList must contain everything in 'toggles'
  // If this triggers, you likely created a new flag and forgot to add it to the implementing class's FlagList

  private val flagMap = flagList map (x => (x, toggles.contains(x))) toMap

  def get(flag: TestToggleFlag) : Boolean = {
    flagMap(flag)
  }

  def getAll : List[TestToggleFlag] = {
    flagMap.filter(_._2 == true).keySet.toList
  }

}
