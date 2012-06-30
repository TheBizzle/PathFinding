package utilitylib.time

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/30/12
 * Time: 1:54 PM
 */

object Time {

  def time[R](block: => R) : R = {
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
  }
  
}
