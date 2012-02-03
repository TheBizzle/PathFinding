package tester.testcluster.testfunction

import java.lang.reflect.Field
import tester.testcluster.{TestSubject, TestCluster}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 1/19/12
 * Time: 7:49 PM
 */

// Uses reflection to do satanical things.  Use at your own risk!
trait TestFunctionFactory[T <: TestFunction[_, U, _, _], U <: TestSubject] {

    self: TestCluster[T, U] =>

    // Essentially, uses reflection to find to find all U-type fields of PathingTestCluster
    def generateTests : List[T] = {

        val generator = generateTestFunction(_: (Field, String), regex = testFunctionRegex)  // Partial application
        val fieldTuples = this.getClass.getDeclaredFields.map { case x => (x, x.getName) }

        fieldTuples.foldRight (List[T]()) { case(x, acc) =>
            generator(x) match {
                case Some(y) => y :: acc
                case None    => acc
            }
        }

    }

    protected def generateTestFunction(fieldData: (Field, String), regex: String) : Option[T]
    protected def construct(subject: U, testNumber: Int, shouldPass: Boolean) : T

}