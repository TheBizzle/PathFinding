package tester.criteria

import collection.mutable.HashMap
import java.security.InvalidParameterException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/20/11
 * Time: 6:51 PM
 */

class TestToggleFlagWrapper(toggles: Option[List[TestCriteriaToggleFlag]]) {

    private val FlagList = List(Talkative, RunBaseTests, SkipPathingTests)

    private val flagMap = initializeFlagMap(FlagList)

    toggles match {
        case None    => throw new InvalidParameterException("OMG, what did you do?!")
        case Some(x) => x map (verifyAndInsert(_))
    } 

    // No entries should be ADDED to the map after calling this once
    private def initializeFlagMap(flags: List[TestToggleFlag]) : HashMap[TestToggleFlag, Boolean] = {
        def initializationHelper(flags: List[TestToggleFlag], flagMap: HashMap[TestToggleFlag, Boolean]) : HashMap[TestToggleFlag, Boolean] = {
            flags match {
                case Nil  => flagMap
                case h::t => { flagMap.put(h, false); initializationHelper(t, flagMap)}
            }
        }
        initializationHelper(flags, new HashMap[TestToggleFlag, Boolean]())
    }

    // No entries should be added—or even UPDATED—to the map after calling this once
    private def verifyAndInsert(crit: TestCriteriaToggleFlag) {
        val flag = crit.criteria
        flagMap.get(flag) match {
            case Some(x) => flagMap.update(flag, true)
            case None => throw new InvalidParameterException("Unknown toggle: " + flag.toString +
                                                             "\nDid you make a new flag and forget to add it to TestToggleFlagWrapper's FlagList?")
        }
    }

    def get(crit: TestToggleFlag) : Boolean = {
        flagMap(crit)
    }

}