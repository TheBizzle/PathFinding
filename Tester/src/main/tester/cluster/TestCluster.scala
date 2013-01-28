package tester.cluster

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/26/11
 * Time: 7:45 PM
 */

trait Testable
trait TestSubject
trait TestCluster[T <: TestFunction[_, U, _, _, _], U <: TestSubject, V <: TestFuncConstructionBundle] extends TestFunctionFactory[T, U, V] {
  protected def TestFunctionRegex                 : String
  /*none */ def size                              : Int
  /*none */ def getTestsToRun(testNums: Seq[Int]) : Seq[T]
}
