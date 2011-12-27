package tester.testcluster

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

// @address Is there a way to do this subtyping of StepData correctly...?
// @address New concern: This is just terrible
abstract class TestFunction[T] extends Function2[T, Boolean, Unit] {
    // Consider not passing the toggle flags to apply() and just batch-processing toggles into function object local variables.
    // That way, the signatures of the functions wouldn't need to change if ever more toggles were added.
    // However... maybe they should HAVE TO change (to be sure of compliance).... :food for thought:
    def apply(pathFinder: T, isTalkative: Boolean)
}