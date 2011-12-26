package pathfinding.tester

import criteria._
import collection.immutable.{HashMap, Map}
import exceptions._
import pathfinding.{StepData, PathFinder}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/19/11
 * Time: 8:27 PM
 */

object TestingCore {

    private[tester] val ArgKeyValue = "value"
    private[tester] val ArgKeyRange = "range"
    private[tester] val ArgKeyToggle = "toggle"

    // These existential types displease me...
    def apply[T <: StepData](args: List[TestCriteria[_]], thingToTest: PathFinder[T]) {
        val argMap = sortArgLists(args)
        makeTestRunningDecisions(argMap, thingToTest)
    }

    private def makeTestRunningDecisions[T <: StepData](argMap: Map[String, List[TestCriteria[_]]], thingToTest: PathFinder[T]) {

        val toggles = new TestToggleFlagWrapper(argMap.get(ArgKeyToggle).asInstanceOf[Option[List[TestCriteriaToggleFlag]]])
        val wantsToRunPathing = assessPathingDesire(argMap)
        val (isTalkative, isRunningBaseTests, isSkippingPathingTests) = (toggles.get(Talkative), toggles.get(RunBaseTests), toggles.get(SkipPathingTests))

        if (isSkippingPathingTests)
            if(wantsToRunPathing)
                throw new ContradictoryArgsException("If you want skip the pathing tests, you should not specify pathing tests to run.")
            else if (!isRunningBaseTests)
                throw new NotRunningTestsException("You can't run the test suite if you're going to skip the pathing tests AND the base tests")

        val testsToRun = {
            if (!isSkippingPathingTests) {

                val valuesOption = argMap.get(ArgKeyValue).asInstanceOf[Option[List[TestCriteriaValueTuple]]]
                val rangesOption = argMap.get(ArgKeyRange).asInstanceOf[Option[List[TestCriteriaRangeTuple]]]

                // Will need some work
                // val (values, ranges) = List(valuesOption, rangesOption) foreach ( _ match { case None => Nil; case Some(x) => sortCriteria(x) } )

                val values = valuesOption  match {
                    case Some(x @ (h::t)) => sortCriteriaValues(x)
                    case _                => Nil
                }

                val ranges = rangesOption match {
                    case Some(x @ (h::t)) => sortCriteriaRanges(x)
                    case _                => Nil
                }

                handleTestIntervals(values, ranges)
                
            }
            else
                Nil
        }

        PathingTestCluster.runTests(testsToRun, thingToTest, isTalkative)

        if (isRunningBaseTests)
            runBaseTests()

    }

    private[tester] def handleTestIntervals(values: List[TestCriteriaValueTuple], ranges: List[TestCriteriaRangeTuple]) : List[Int] = {

        if (!values.isEmpty && (values.last.criteria.guide > PathingTestCluster.getSize))
            throw new InvalidTestNumberException("There is no test #" + values.last.criteria.guide)

        val unionOfRanges = {
            if (!ranges.isEmpty) {
                // In a strange and unprecedented act of grace, I'll just ignore invalid ranges
                val union = findUnionOfIntervals(ranges).foldLeft (List[TestCriteriaRangeTuple]()) ( (acc, x) => if (x.isValid) x :: acc else acc ).reverse
                if (!union.isEmpty && (union.last.criteria.guide._2 > PathingTestCluster.getSize))
                    throw new InvalidTestNumberException("Test range (" + union.last.criteria.guide._1 + ", " + union.last.criteria.guide._2 +
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
            throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipPathingTests flag, instead!")

        val resultArr = mergeValuesIntoArr(createArrayFromRangeCriteriaList(unionOfRanges, overallMax), values)
        val outList = { for ( i <- 0 until resultArr.size;
                              if (resultArr(i)) ) yield i }.toList

        if (!outList.isEmpty)
            outList
        else
            throw new NotRunningTestsException("All runnable tests were excluded!  Use the SkipPathingTests flag, instead!")

    }

    private def runBaseTests() {
        // Probably just call execute() on a ScalaTest class or something
    }

    private[tester] def assessPathingDesire(argMap:  Map[String, List[TestCriteria[_]]]) : Boolean = {
        def hasPathingDesire(h: TestCriteria[_]) : Boolean = {
            h.asInstanceOf[TestCriteria[TestTuple[_,_]]].criteria.flag.isInstanceOf[TestRunFlag]
        }
        argMap(ArgKeyValue).exists(hasPathingDesire) || argMap(ArgKeyRange).exists(hasPathingDesire)
    }

    private[tester] def sortArgLists(args: List[TestCriteria[_]]) : Map[String, List[TestCriteria[_]]] = {
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
        sortHelper(args, HashMap[String, List[TestCriteria[_]]](ArgKeyValue  -> List[TestCriteriaValueTuple](),
                                                                ArgKeyRange  -> List[TestCriteriaRangeTuple](),
                                                                ArgKeyToggle -> List[TestCriteriaToggleFlag]()))
    }

    // There's a way to avoid needing two functions that do the same thing with different parameters—think implicit conversions!
    private[tester] def sortCriteriaValues(criteriaList: List[TestCriteriaValueTuple]) : List[TestCriteriaValueTuple] = {
        criteriaList.sortWith((a, b) => a.criteria.guide < b.criteria.guide)
    }

    private[tester] def sortCriteriaRanges(criteriaList: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {
        criteriaList.sortWith((a, b) => a.criteria.guide._1 < b.criteria.guide._1)
    }

    private[tester] def findUnionOfIntervals(ranges: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {
        val (testList, skipList) = siftOutTestsAndSkips(ranges)
        coalesceLists(condenseCriteriaTupleList(testList), condenseCriteriaTupleList(skipList))
    }
    
    private[tester] def siftOutTestsAndSkips(list: List[TestCriteriaRangeTuple]) : (List[TestCriteriaRangeTuple], List[TestCriteriaRangeTuple]) = {
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
                        throw new UnexpectedTypeException("Unexpected type of CriteriaRangeTuple!")   // EXPLODE!
                }
            }
        }
        siftHelper(list, List[TestCriteriaRangeTuple](), List[TestCriteriaRangeTuple]())
    }

    private[tester] def condenseCriteriaTupleList(ranges: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {

        def condensationHelper(ranges: List[TestCriteriaRangeTuple], r: TestCriteriaRangeTuple) : List[TestCriteriaRangeTuple] = {
            ranges match {
                case Nil  => List(r)
                case h::t => {
                    if ((r encapsulates h) || (h encapsulates r))
                        throw new FullEncapsulationException("Range (" + r.criteria.guide._1 + ", " + r.criteria.guide._2 + ") and " +
                                                             "range (" + h.criteria.guide._1 + ", " + h.criteria.guide._2 + ") form a full encapsulation")
                    else if (r intersects h)
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

    private[tester] def coalesceLists(a: List[TestCriteriaRangeTuple], b: List[TestCriteriaRangeTuple]) : List[TestCriteriaRangeTuple] = {

        def trimLists(a: List[TestCriteriaRangeTuple], bh: TestCriteriaRangeTuple) : List[TestCriteriaRangeTuple] = {
            a match {
                case Nil    => throw new UnnecessaryExclusionException("Exclusion of range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ") is unnecessary")
                case ah::at => {
                    if(ah intersects bh)
                        if (bh encapsulates ah)
                            throw new FullEncapsulationException("Full encapsulation of range (" + ah.criteria.guide._1 + ", " + ah.criteria.guide._2 + ") " +
                                                                 "by range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ")")
                        else
                            TestCriteriaRangeTuple(bh.criteria.guide._2 + 1, ah.criteria.guide._2, ah.criteria.flag) :: at
                    else
                        a
                }
            }
        }

        b match {
            case Nil    => a
            case bh::bt => {
                a match {
                    case Nil    => throw new UnnecessaryExclusionException("Exclusion of range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ") is unnecessary")
                    case ah::at => {
                        if (ah intersects bh) {
                            val ahl = TestCriteriaRangeTuple(ah.criteria.guide._1, bh.criteria.guide._1 - 1, ah.criteria.flag)
                            if (ah encapsulates bh) {
                                val ahr = TestCriteriaRangeTuple(bh.criteria.guide._2 + 1, ah.criteria.guide._2, ah.criteria.flag)
                                ahl :: coalesceLists(ahr :: at, bt)
                            }
                            else if (bh encapsulates ah) {
                                throw new FullEncapsulationException("Full encapsulation of range (" + ah.criteria.guide._1 + ", " + ah.criteria.guide._2 + ") " +
                                                                 "by range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ")")
                            }
                            else
                                ahl :: coalesceLists(trimLists(at, bh), bt)
                        }
                        else
                            throw new UnnecessaryExclusionException("Exclusion of range (" + bh.criteria.guide._1 + ", " + bh.criteria.guide._2 + ") is unnecessary")
                    }
                }
            }
        }

    }

    private[tester] def mergeCriteriaRangeTuples(a: TestCriteriaRangeTuple, b: TestCriteriaRangeTuple) : TestCriteriaRangeTuple = {

        val (firstCrit, secondCrit) = {
            if (b.criteria.guide._1 < a.criteria.guide._1)
                (a.criteria, b.criteria)
            else
                (b.criteria, a.criteria)
        }

        if (firstCrit.flag != secondCrit.flag)
            throw new RedundantInclusionException("Flag mismatch on merging tuple " + a + " and tuple " + b)

        if (secondCrit.guide._2 < firstCrit.guide._2)
            throw new FullEncapsulationException("Erroneous fully-encapsulated interval (" + secondCrit.guide._1 + ", " + secondCrit.guide._2 + ") supplied!")

        TestCriteriaRangeTuple(firstCrit.guide._1, secondCrit.guide._2, firstCrit.flag)

    }

    // Basically, takes advantage of bucketing to quickly deal with test numbers and their test-ness/skip-ness
    // Generally, calls an implicit conversion of List[RangeTuples] into List[List[ValueTuple]]s where it is called
    private[tester] def createArrayFromRangeCriteriaList(ranges: List[List[TestCriteriaValueTuple]], maxNum: Int) : Array[Boolean] = {
        val arr = new Array[Boolean](maxNum + 1)
        ranges.flatten.foreach { x => arr(x.criteria.guide) = true }
        arr
    }

    private[tester] def mergeValuesIntoArr(rangesArr: Array[Boolean], values: List[TestCriteriaValueTuple]) : Array[Boolean] = {

        def addToArr(rangesArr: Array[Boolean], value: Int) : Array[Boolean] = {
            if (rangesArr(value))
                throw new RedundantInclusionException("Redundant inclusion of test #" + value)
            else {
                rangesArr(value) = true
                rangesArr
            }
        }

        def removeFromArr(rangesArr: Array[Boolean], value: Int) : Array[Boolean] = {
            if (rangesArr(value)) {
                rangesArr(value) = false
                rangesArr
            }
            else
                throw new UnnecessaryExclusionException("Redundant exclusion of test #" + value)
        }

        values match {
            case Nil  => rangesArr
            case h::t => {
                import h.criteria._
                if (flag.isInstanceOf[TestRunFlag])
                    addToArr(rangesArr, guide)
                else if (flag.isInstanceOf[TestSkipFlag])
                    removeFromArr(rangesArr, guide)
                else
                    throw new UnexpectedTypeException("Unexpected type of CriteriaRangeTuple!")   // EXPLODE!
                mergeValuesIntoArr(rangesArr, t)
            }
        }
        
    }

}