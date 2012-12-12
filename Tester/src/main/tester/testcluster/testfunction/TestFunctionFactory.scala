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
trait TestFunctionFactory[T <: TestFunction[_, U, _, _, _], U <: TestSubject, V <: TestFuncConstructionBundle] {

  self: TestCluster[T, U, V] =>

  // Essentially, uses reflection to find to find all T-type fields of PathingTestCluster
  def generateTests : Seq[T] = {
    val generator = generateTestFunction(_: (Field, String), regex = TestFunctionRegex)  // Partial application
    val fieldTuples = this.getClass.getDeclaredFields map (x => (x, x.getName))
    fieldTuples map(generator(_)) collect { case Some(x) => x } toSeq
  }

  protected def generateTestFunction(fieldData: (Field, String), regex: String) : Option[T]
  protected def construct(subject: U, testNumber: Int, shouldPass: Boolean, bundle: V) : T

}
