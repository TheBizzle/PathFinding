package tester.criteria

import collection.mutable.HashMap
import annotation.tailrec
import tester.exceptions.MysteriousDataException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/2/12
 * Time: 12:45 AM
 */

class BaseToggleFlagWrapper(toggles: List[TestToggleFlag], supportedToggles: List[TestToggleFlag]) {

    private val FlagList = supportedToggles

    private val flagMap = initializeFlagMap(FlagList)
    toggles map ( verifyAndInsert(_) )

    // No entries should be ADDED to the map after calling this once
    private def initializeFlagMap(flags: List[TestToggleFlag]) : HashMap[TestToggleFlag, Boolean] = {
        @tailrec def initializationHelper(flags: List[TestToggleFlag], flagMap: HashMap[TestToggleFlag, Boolean]) : HashMap[TestToggleFlag, Boolean] = {
            flags match {
                case Nil  => flagMap
                case h::t => flagMap.put(h, false); initializationHelper(t, flagMap)
            }
        }
        initializationHelper(flags, new HashMap[TestToggleFlag, Boolean]())
    }

    // No entries should be added—or even UPDATED—to the map after calling this once
    private def verifyAndInsert(flag: TestToggleFlag) {
        flagMap.get(flag) match {
            case Some(x) => flagMap.update(flag, true)
            case None => throw new MysteriousDataException("Unknown toggle: " + flag.toString +
                                                           "\nDid you make a new flag and forget to add it to " + this.getClass.getName + "'s FlagList?")
        }
    }

    def get(flag: TestToggleFlag) : Boolean = {
        flagMap(flag)
    }

    def getAll : List[TestToggleFlag] = {
        flagMap.filter( _._2 == true ).keySet.toList
    }

}
