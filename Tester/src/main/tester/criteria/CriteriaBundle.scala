package tester.criteria

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 2/6/13
 * Time: 6:48 PM
 */

case class CriteriaBundle(toggles: Seq[TestCriteriaToggleFlag] = Seq(), values: Seq[TestRunningnessValue] = Seq(), ranges: Seq[TestRunningnessRange] = Seq())
