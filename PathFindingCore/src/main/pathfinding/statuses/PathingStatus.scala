package pathfinding.statuses

import pathfinding.StepData
import tester.testanalyzer.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:37 PM
 */

sealed abstract class PathingStatus[+T <: StepData](val stepData: T) extends ExecutionStatus

case class Continue[+T <: StepData](sd: T) extends PathingStatus[T](sd)
case class Success[+T <: StepData](sd: T) extends PathingStatus[T](sd)
case class Failure[+T <: StepData](sd: T) extends PathingStatus[T](sd)
