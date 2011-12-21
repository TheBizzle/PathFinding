package astar_base.tester

import criteria._
import collection.immutable.{TreeMap, HashMap, Map}
import java.security.InvalidParameterException

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

    private def findUnionOfIntervals(ranges: List[PathingTestCriteriaRangeTuple]) : List[PathingTestCriteriaRangeTuple] = {
        val (testList, skipList) = siftTestAndSkip(ranges)
        coalesceLists(condenseCriteriaTupleList(testList), condenseCriteriaTupleList(skipList))
    }
    
    private def siftTestAndSkip(list: List[PathingTestCriteriaRangeTuple]) : (List[PathingTestCriteriaRangeTuple], List[PathingTestCriteriaRangeTuple]) = {
        def siftHelper(inList: List[PathingTestCriteriaRangeTuple], testList: List[PathingTestCriteriaRangeTuple], skipList: List[PathingTestCriteriaRangeTuple]) : (List[PathingTestCriteriaRangeTuple], List[PathingTestCriteriaRangeTuple]) = {
            inList match {
                case Nil                  => (testList, skipList)
                case h::t                 => {
                    val flag = h.criteria.flag
                    if (flag.isInstanceOf[TestRunFlag])
                        siftHelper(t, h :: testList, skipList)
                    else if (flag.isInstanceOf[TestSkipFlag])
                        siftHelper(t, testList, h :: skipList)
                    else
                        throw new InvalidParameterException("Unexpected type of CriteriaRangeTuple!")   // EXPLODE!
                }
            }
        }
        siftHelper(list, List[PathingTestCriteriaRangeTuple](), List[PathingTestCriteriaRangeTuple]())
    }

    private def condenseCriteriaTupleList(ranges: List[PathingTestCriteriaRangeTuple]) : List[PathingTestCriteriaRangeTuple] = {
        def condensationHelper(ranges: List[PathingTestCriteriaRangeTuple], r: PathingTestCriteriaRangeTuple) : List[PathingTestCriteriaRangeTuple] = {
            ranges match {
                case Nil  => Nil
                case h::t => {
                    if (doIntersect(r, h))
                        condensationHelper(t, mergeCriteriaRangeTuples(r, h))   // If the first two intersect, merge and recurse
                    else
                        r :: condensationHelper(t, h)                           // If they don't, r is fully condensed
                }
            }
        }
        condensationHelper(ranges.tail, ranges.head)
    }

    private def coalesceLists(a: List[PathingTestCriteriaRangeTuple], b: List[PathingTestCriteriaRangeTuple]) : List[PathingTestCriteriaRangeTuple] = {
        // Compare a.head and b.head
        // If they intersect...
        // If they don't intersect...
        // Crazy, tricky, "watch the order!"-style logic!
        null
    }

    private def doIntersect(a: PathingTestCriteriaRangeTuple, b: PathingTestCriteriaRangeTuple) : Boolean = {
        val ra = Range(a.criteria.guide._1, a.criteria.guide._2)
        val rb = Range(b.criteria.guide._1, b.criteria.guide._2)
        ra.intersect(rb).size > 0
    }

    // Coming into things, it's assumed that a starts at (or before) b
    private def mergeCriteriaRangeTuples(a: PathingTestCriteriaRangeTuple, b: PathingTestCriteriaRangeTuple) : PathingTestCriteriaRangeTuple = {

        val aCrit = a.criteria
        val bCrit = b.criteria

        if (aCrit.flag != bCrit.flag)
            throw new InvalidParameterException("Flag mismatch on merging tuple " + a + " and tuple " + b)

        if (bCrit.guide._2 < aCrit.guide._2)
            throw new InvalidParameterException("Erroneous fully-encapsulated interval (" + bCrit.guide._1 + ", " + bCrit.guide._2 + ") supplied!")

        PathingTestCriteriaRangeTuple(aCrit.guide._1, bCrit.guide._2, aCrit.flag)

    }

    // Calls an implicit conversion of RangeTuples into List[ValueTuple]s
    private def createTreeFromRangeCriteriaList(ranges: List[PathingTestCriteriaRangeTuple]) : TreeMap[Int, Boolean] = {
        ranges.flatten.foldLeft (TreeMap[Int, Boolean]()) ( (tree, x) => tree + (x.criteria.guide -> true) )
    }

    private def mergeTreeWithValues(rangesTree: TreeMap[Int, Boolean], values: List[PathingTestCriteriaValueTuple]) : TreeMap[Int, Boolean] = {

        def addToTree(rangesTree: TreeMap[Int, Boolean], value: Int) : TreeMap[Int, Boolean] = {
            rangesTree.get(value) match {
                case Some(_) => throw new InvalidParameterException("Redundant inclusion of test #" + value)
                case None    => rangesTree + (value -> true)
            }
        }

        def removeFromTree(rangesTree: TreeMap[Int, Boolean], value: Int) : TreeMap[Int, Boolean] = {
            rangesTree.get(value) match {
                case Some(_) => rangesTree - value
                case None    => throw new InvalidParameterException("Redundant exclusion of test #" + value)
            }
        }

        values match {
            case Nil  => rangesTree
            case h::t => {
                val flag = h.criteria.flag
                val neoTree = {
                    if (flag.isInstanceOf[TestRunFlag])
                        addToTree(rangesTree, h.criteria.guide)
                    else if (flag.isInstanceOf[TestSkipFlag])
                        removeFromTree(rangesTree, h.criteria.guide)
                    else
                        throw new InvalidParameterException("Unexpected type of CriteriaRangeTuple!")   // EXPLODE!
                }
                mergeTreeWithValues(neoTree, t)
            }
        }
        
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