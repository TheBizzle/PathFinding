package astar_base.tester

import criteria._
import java.security.InvalidParameterException
import collection.immutable.{TreeMap, HashMap, Map}

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
        val argMap = sortArgLists(args)
        val relevantKeys = findRelevantKeys(argMap)
        makeTestRunningDecisions(argMap, relevantKeys)
    }

    private def makeTestRunningDecisions(argMap: Map[String, List[PathingTestCriteria]], keyList: List[String]) {

        val toggles = new TestToggleFlagWrapper(argMap.get(ArgKeyToggle).asInstanceOf[Option[List[PathingTestCriteriaToggleFlag]]])
        val wantsToRunPathing = assessPathingDesire(argMap)
        val (isTalkative, isRunningBaseTests, isSkippingPathingTests) = (toggles.get(Talkative), toggles.get(RunBaseTests), toggles.get(SkipPathingTests))

        if ((isSkippingPathingTests && (wantsToRunPathing || !isRunningBaseTests)))
            throw new InvalidParameterException

        val testsToRun = {
            if (!isSkippingPathingTests)
                handleTestIntervals(argMap.get(ArgKeyValue).asInstanceOf[Option[List[PathingTestCriteriaValueTuple]]],
                                    argMap.get(ArgKeyRange).asInstanceOf[Option[List[PathingTestCriteriaRangeTuple]]])
            else
                Nil
        }

        runPathingTests(testsToRun, isTalkative)

        if (isRunningBaseTests)
            runBaseTests()

    }

    private def handleTestIntervals(valuesOption: Option[List[PathingTestCriteriaValueTuple]], rangesOption: Option[List[PathingTestCriteriaRangeTuple]]) : List[Int] = {

        val (values, ranges) = List(valuesOption, rangesOption) foreach ( _ match { case None => Nil; case Some(_) => sortCriteria(_) } )

        if (values.last.criteria.guide > getMaxTestNum)
            throw new InvalidParameterException("There is no test #" + values.last.criteria.guide)

        val unionOfRanges = findUnionOfIntervals(ranges)

        if (unionOfRanges.last.criteria.guide._2 > getMaxTestNum)
            throw new InvalidParameterException("Test range (" + unionOfRanges.last.criteria.guide._1 + ", " + unionOfRanges.last.criteria.guide._2 +
                                                " extends to a number for which there is no corresponding test")

        val outList = mergeTreeWithValues(createTreeFromRangeCriteriaList(unionOfRanges), values).toList

        if (!outList.isEmpty)
            outList map ( _._1 )
        else
            throw new InvalidParameterException("No tests to run!")

    }

    private def runPathingTests(testsToRun: List[Int], isTalkative: Boolean) {
        val testArray = initializeTestArray
        testsToRun foreach ( testArray(_)/*(isTalkative)*/ )
    }

    private def runBaseTests() {
        // Probably just call execute() on a ScalaTest class or something
    }

    private def assessPathingDesire(argMap:  Map[String, List[PathingTestCriteria]]) : Boolean = {
        def assessmentHelper(argListOption: Option[List[PathingTestCriteria]]) : Boolean = {
            argListOption match {
                case None    => false
                case Some(x) => x foreach { x => if (x.isInstanceOf[TestRunFlag]) true }; false
            }
        }
        assessmentHelper(argMap.get(ArgKeyValue)) || assessmentHelper(argMap.get(ArgKeyRange))
    }

    private def sortArgLists(args: List[PathingTestCriteria]) : Map[String, List[PathingTestCriteria]] = {
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
                    sortHelper(t, argMap + (key -> (h :: argMap(key))))
                }
            }
        }
        sortHelper(args, HashMap[String, List[PathingTestCriteria]](ArgKeyValue -> List[PathingTestCriteriaValueTuple](),
                                                                    ArgKeyRange -> List[PathingTestCriteriaRangeTuple](),
                                                                    ArgKeyToggle -> List[PathingTestCriteriaToggleFlag]()))
    }

    private def findRelevantKeys(argMap: Map[String, List[PathingTestCriteria]]) : List[String] = {
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

    private def sortCriteria[T <: PathingTestCriteria](criteriaList: List[T]) : List[T] = {
        criteriaList match {
            case x: List[PathingTestCriteriaValueTuple] => x.sortWith((a, b) => a.criteria.guide < b.criteria.guide)
            case x: List[PathingTestCriteriaRangeTuple] => x.sortWith((a, b) => a.criteria.guide._1 < b.criteria.guide._1)
            case _                                      => throw new InvalidParameterException
        }
    }

    def findUnionOfIntervals(ranges: List[PathingTestCriteriaRangeTuple]) : List[PathingTestCriteriaRangeTuple] = {
        // Make it!
        // -Error on interval consumption
        // -Error on useless removal range
        null
    }

    // Calls an implicit conversion of RangeTuples into List[ValueTuple]s
    def createTreeFromRangeCriteriaList(ranges: List[PathingTestCriteriaRangeTuple]) : TreeMap[Int, Boolean] = {
        ranges.flatten foldLeft(TreeMap[Int, Boolean]())( (tree, x) => tree + (x.criteria.guide -> true) )
    }

    def mergeTreeWithValues(rangesMap: TreeMap[Int, Boolean], values: List[PathingTestCriteriaValueTuple]) : TreeMap[Int, Boolean] = {
        // Make it!
        // -   Add   (Success) : Add it...
        // -   Add   (Failure) : "Error!  Duplicate!"
        // - Removal (Success) : Remove it...
        // - Removal (Failure) : "Error!  Nothing to remove!"
        null
    }

    private def initializeTestArray : Array[Function] = {
        // Add in some time—or probably delegate this to some class that wraps all of the test functions
        Array[Function]()
    }

    private def getMaxTestNum : Int = {
        // Add something into the testrunner module for calculating this, then delete this and call that
        37
    }

}