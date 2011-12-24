package pathfinding.tester

import criteria._
import collection.immutable.{HashMap, Map}
import java.security.InvalidParameterException
import pathfinding.{StepData, PathFinder}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:27 PM
 */

object TestingCore {

    private val ArgKeyValue = "value"
    private val ArgKeyRange = "range"
    private val ArgKeyToggle = "toggle"
    private val ArgKeyList = List(ArgKeyToggle, ArgKeyRange, ArgKeyValue)   // The ordering here is important.  Don't mess with it!

    // These existential types displease me...
    def apply[T <: StepData](args: List[TestCriteria[_]], thingToTest: PathFinder[T]) {
        val argMap = sortArgLists(args)
        val relevantKeys = findRelevantKeys(argMap)
        makeTestRunningDecisions(argMap, relevantKeys, thingToTest)
    }

    private def makeTestRunningDecisions[T <: StepData](argMap: Map[String, List[TestCriteria[_]]], keyList: List[String], thingToTest: PathFinder[T]) {

        val toggles = new TestToggleFlagWrapper(argMap.get(ArgKeyToggle).asInstanceOf[Option[List[TestCriteriaToggleFlag]]])
        val wantsToRunPathing = assessPathingDesire(argMap)
        val (isTalkative, isRunningBaseTests, isSkippingPathingTests) = (toggles.get(Talkative), toggles.get(RunBaseTests), toggles.get(SkipPathingTests))

        if ((isSkippingPathingTests && (wantsToRunPathing || !isRunningBaseTests)))
            throw new InvalidParameterException

        val testsToRun = {
            if (!isSkippingPathingTests)
                handleTestIntervals(argMap.get(ArgKeyValue).asInstanceOf[Option[List[TestCriteriaValueTuple]]],
                                    argMap.get(ArgKeyRange).asInstanceOf[Option[List[TestCriteriaRangeTuple]]])
            else
                Nil
        }

        PathingTestCluster.runTests(testsToRun, thingToTest, isTalkative)

        if (isRunningBaseTests)
            runBaseTests()

    }

    private def handleTestIntervals(valuesOption: Option[List[TestCriteriaValueTuple]], rangesOption: Option[List[TestCriteriaRangeTuple]]) : List[Int] = {

        // Will need some work
        // val (values, ranges) = List(valuesOption, rangesOption) foreach ( _ match { case None => Nil; case Some(x) => sortCriteria(x) } )
        
        val values = valuesOption match {
            case None    => Nil
            case Some(x) => sortCriteriaValues(x)
        }

        val ranges = rangesOption match {
            case None    => Nil
            case Some(x) => sortCriteriaRanges(x)
        }

        if (!values.isEmpty && (values.last.criteria.guide > PathingTestCluster.getSize))
            throw new InvalidParameterException("There is no test #" + values.last.criteria.guide)

        val unionOfRanges = {
            if (!ranges.isEmpty) {
                val union = findUnionOfIntervals(ranges).foldLeft (List[TestCriteriaRangeTuple]()) ( (acc, x) => if (x.isValid) x :: acc else acc )
                if (!union.isEmpty && (union.last.criteria.guide._2 > PathingTestCluster.getSize))
                    throw new InvalidParameterException("Test range (" + union.last.criteria.guide._1 + ", " + union.last.criteria.guide._2 +
                                                        " extends to a number for which there is no corresponding test")
                union
            }
            else
                Nil
        }

        val maxVal = if (!values.isEmpty) values.last.criteria.guide else 0
        val maxRange = if (!unionOfRanges.isEmpty) unionOfRanges.last.criteria.guide._2 else 0
        val overallMax = if (maxVal > maxRange) maxVal else maxRange

        if (overallMax < 1)
            throw new InvalidParameterException("All runnable tests were excluded!  Use the SkipPathingTests flag, instead!")

        val resultArr = mergeValuesIntoArr(createArrayFromRangeCriteriaList(unionOfRanges, overallMax), values)
        val outList = { for ( i <- 0 until resultArr.size;
                              if (resultArr(i)) ) yield i }.toList

        if (!outList.isEmpty)
            outList
        else
            throw new InvalidParameterException("All runnable tests were excluded!  Use the SkipPathingTests flag, instead!")

    }

    private def runBaseTests() {
        // Probably just call execute() on a ScalaTest class or something
    }

    private def assessPathingDesire(argMap:  Map[String, List[TestCriteria[_]]]) : Boolean = {
        def assessmentHelper(argListOption: Option[List[TestCriteria[_]]]) : Boolean = {
            argListOption match {
                case None    => false
                case Some(x) => x foreach { x => if (x.isInstanceOf[TestRunFlag]) true }; false
            }
        }
        assessmentHelper(argMap.get(ArgKeyValue)) || assessmentHelper(argMap.get(ArgKeyRange))
    }

    private def sortArgLists(args: List[TestCriteria[_]]) : Map[String, List[TestCriteria[_]]] = {
        def sortHelper(args: List[TestCriteria[_]], argMap: Map[String, List[TestCriteria[_]]]) : Map[String, List[TestCriteria[_]]] = {
            args match {
                case Nil  => argMap
                case h::t => {
                    val key = {
                        h match {
                            case x: TestCriteriaValueTuple => ArgKeyValue
                            case x: TestCriteriaRangeTuple => ArgKeyRange
                            case x: TestCriteriaToggleFlag => ArgKeyToggle
                        }
                    }
                    sortHelper(t, argMap + (key -> (h :: argMap(key))))
                }
            }
        }
        sortHelper(args, HashMap[String, List[TestCriteria[_]]](ArgKeyValue -> List[TestCriteriaValueTuple](),
                                                                ArgKeyRange -> List[TestCriteriaRangeTuple](),
                                                                ArgKeyToggle -> List[TestCriteriaToggleFlag]()))
    }

    private def findRelevantKeys(argMap: Map[String, List[TestCriteria[_]]]) : List[String] = {
        def relevantKeyHelper(argMap: Map[String, List[TestCriteria[_]]], keyList: List[String]) : List[String] = {
            keyList match {
                case Nil  => Nil
                case h::t =>
                    argMap.get(h) match {
                        case Some(x: List[TestCriteria[_]]) => if (!x.isEmpty) h :: relevantKeyHelper(argMap, t) else Nil
                        case None    => Nil
                    }
            }
        }
        relevantKeyHelper(argMap, ArgKeyList)
    }

    // There's a better way to do this—think implicit conversions!
    private def sortCriteriaValues(criteriaList: List[TestCriteriaValueTuple]) : List[TestCriteriaValueTuple] = {
        criteriaList.sortWith((a, b) => a.criteria.guide < b.criteria.guide)
    }

    private def sortCriteriaRanges(criteriaList: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {
        criteriaList.sortWith((a, b) => a.criteria.guide._1 < b.criteria.guide._1)
    }

    private def findUnionOfIntervals(ranges: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {
        val (testList, skipList) = siftOutTestsAndSkips(ranges)
        coalesceLists(condenseCriteriaTupleList(testList), condenseCriteriaTupleList(skipList))
    }
    
    private def siftOutTestsAndSkips(list: List[TestCriteriaRangeTuple]) : (List[TestCriteriaRangeTuple], List[TestCriteriaRangeTuple]) = {
        def siftHelper(inList: List[TestCriteriaRangeTuple], testList: List[TestCriteriaRangeTuple], skipList: List[TestCriteriaRangeTuple]) : (List[TestCriteriaRangeTuple], List[TestCriteriaRangeTuple]) = {
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
        siftHelper(list, List[TestCriteriaRangeTuple](), List[TestCriteriaRangeTuple]())
    }

    private def condenseCriteriaTupleList(ranges: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {

        def condensationHelper(ranges: List[TestCriteriaRangeTuple], r: TestCriteriaRangeTuple) : List[TestCriteriaRangeTuple] = {
            ranges match {
                case Nil  => Nil
                case h::t => {
                    if (r intersects h)
                        condensationHelper(t, mergeCriteriaRangeTuples(r, h))   // If the first two intersect, merge and recurse
                    else
                        r :: condensationHelper(t, h)                           // If they don't, r is fully condensed
                }
            }
        }

        ranges match {
            case Nil  => Nil
            case h::t => condensationHelper(t, h)
        }

    }

    private def coalesceLists(a: List[TestCriteriaRangeTuple], b: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {

        def trimOffEncapsulateds(a: List[TestCriteriaRangeTuple], bh: TestCriteriaRangeTuple) : List[TestCriteriaRangeTuple] = {
            a match {
                case Nil    => throw new InvalidParameterException("Exclusion of range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ") is unnecessary")
                case ah::at => {
                    if(ah intersects bh) {
                        if (bh encapsulates ah)
                            trimOffEncapsulateds(at, bh)
                        else {
                            import ah.criteria._
                            val ahr = TestCriteriaRangeTuple(bh.criteria.guide._1 + 1, guide._2, flag)
                            ahr :: at
                        }
                    }
                    else
                        a
                }
            }
        }

        b match {
            case Nil    => Nil
            case bh::bt => {
                a match {
                    case Nil    => throw new InvalidParameterException("Exclusion of range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ") is unnecessary")
                    case ah::at => {
                        if (ah intersects bh) {
                            val ahl = TestCriteriaRangeTuple(ah.criteria.guide._1, bh.criteria.guide._2 - 1, ah.criteria.flag)
                            if (ah encapsulates bh) {
                                val ahr = TestCriteriaRangeTuple(bh.criteria.guide._1 + 1, ah.criteria.guide._2, ah.criteria.flag)
                                ahl :: coalesceLists(ahr :: at, bt)
                            }
                            else {
                                val neoA = trimOffEncapsulateds(at, bh)
                                ahl :: coalesceLists (neoA, bt)
                            }
                        }
                        else
                            throw new InvalidParameterException("Exclusion of range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ") is unnecessary")
                    }
                }
            }
        }

    }



    // Coming into things, it's assumed that a starts at (or before) b
    private def mergeCriteriaRangeTuples(a: TestCriteriaRangeTuple, b: TestCriteriaRangeTuple) : TestCriteriaRangeTuple = {

        val aCrit = a.criteria
        val bCrit = b.criteria

        if (aCrit.flag != bCrit.flag)
            throw new InvalidParameterException("Flag mismatch on merging tuple " + a + " and tuple " + b)

        if (bCrit.guide._2 < aCrit.guide._2)
            throw new InvalidParameterException("Erroneous fully-encapsulated interval (" + bCrit.guide._1 + ", " + bCrit.guide._2 + ") supplied!")

        TestCriteriaRangeTuple(aCrit.guide._1, bCrit.guide._2, aCrit.flag)

    }

    // Basically, takes advantage of bucketing to quickly deal with test numbers and their test-ness/skip-ness
    // Generally, calls an implicit conversion of List[RangeTuples] into List[List[ValueTuple]]s where it is called
    private def createArrayFromRangeCriteriaList(ranges: List[List[TestCriteriaValueTuple]], maxNum: Int) : Array[Boolean] = {
        val arr = new Array[Boolean](maxNum)
        ranges.flatten.foreach { x => arr(x.criteria.guide) = true }
        arr
    }

    private def mergeValuesIntoArr(rangesArr: Array[Boolean], values: List[TestCriteriaValueTuple]) : Array[Boolean] = {

        def addToArr(rangesArr: Array[Boolean], value: Int) : Array[Boolean] = {
            if (rangesArr(value))
                throw new InvalidParameterException("Redundant inclusion of test #" + value)
            else
                { rangesArr(value) = true; rangesArr }
        }

        def removeFromArr(rangesArr: Array[Boolean], value: Int) : Array[Boolean] = {
            if (rangesArr(value))
                { rangesArr(value) = false; rangesArr }
            else
                throw new InvalidParameterException("Redundant exclusion of test #" + value)
        }

        values match {
            case Nil  => rangesArr
            case h::t => {
                val flag = h.criteria.flag
                if (flag.isInstanceOf[TestRunFlag])
                    addToArr(rangesArr, h.criteria.guide)
                else if (flag.isInstanceOf[TestSkipFlag])
                    removeFromArr(rangesArr, h.criteria.guide)
                else
                    throw new InvalidParameterException("Unexpected type of CriteriaRangeTuple!")   // EXPLODE!
                mergeValuesIntoArr(rangesArr, t)
            }
        }
        
    }

}