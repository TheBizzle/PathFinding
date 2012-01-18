package tester.testcluster

import tester.Testable

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

// @address Is there a way to do this subtyping of StepData correctly...?
// @address New concern: This is just terrible
abstract class TestFunction[T <: Testable](testNumber: Int, shouldPass: Boolean = true) extends Function2[T, Boolean, Boolean] {
    // Consider not passing the toggle flags to apply() and just batch-processing toggles into function object local variables.
    // That way, the signatures of the functions wouldn't need to change if ever more toggles were added.
    // However... maybe they should HAVE TO change (to be sure of compliance).... :food for thought:
    val testNum = testNumber
    val shouldSucceed = shouldPass
    def apply(testable: T, isTalkative: Boolean) : Boolean
}