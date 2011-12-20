package astar_base.tester

import collection.immutable.HashMap
import collection.immutable.Map
import criteria._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:27 PM
 */

object PathingTestCore {

    private val ArgKeyValue = "value"
    private val ArgKeyRange = "range"
    private val ArgKeyToggle = "toggle"
    private val ArgKeyList = List(ArgKeyToggle, ArgKeyRange, ArgKeyValue)   // The ordering here is important.  Don't mess with it!

    def apply(args: List[PathingTestCriteria]) {
        def applyHelper(argMap: Map[String, List[PathingTestCriteria]], keyList: List[String]) {
            // What to do...
        }
        val argMap = sortArgs(args)
        val relevantKeys = findRelevantKeys(argMap)
        applyHelper(argMap, relevantKeys)
    }

    def sortArgs(args: List[PathingTestCriteria]) : Map[String, List[PathingTestCriteria]] = {
        def sortHelper(args: List[PathingTestCriteria], argMap: Map[String, List[PathingTestCriteria]]) : Map[String, List[PathingTestCriteria]] = {
            args match {
                case Nil  => argMap
                case h::t => {
                    val key = {
                        h match {
                            case x: PathingTestCriteriaValueTuple => ArgKeyValue
                            case x: PathingTestCriteriaRangeTuple => ArgKeyRange
                            case x: PathingTestCriteriaToggleFlag => ArgKeyToggle
                        }
                    }
                    sortHelper(t, argMap + (key -> (h :: argMap.get(key).get)))
                }
            }
        }
        sortHelper(args, HashMap[String, List[PathingTestCriteria]](ArgKeyValue -> List[PathingTestCriteriaValueTuple](),
                                                                    ArgKeyRange -> List[PathingTestCriteriaRangeTuple](),
                                                                    ArgKeyToggle -> List[PathingTestCriteriaToggleFlag]()))
    }

    def findRelevantKeys(argMap: Map[String, List[PathingTestCriteria]]) : List[String] = {
        def relevantKeyHelper(argMap: Map[String, List[PathingTestCriteria]], keyList: List[String]) : List[String] = {
            keyList match {
                case Nil  => Nil
                case h::t =>
                    argMap.get(h) match {
                        case Some(_) => h :: relevantKeyHelper(argMap, t)
                        case None    => Nil
                    }
            }
        }
        relevantKeyHelper(argMap, ArgKeyList)
    }

}