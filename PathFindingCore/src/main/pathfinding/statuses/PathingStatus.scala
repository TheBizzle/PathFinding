package pathfinding.statuses

import pathfinding.StepData
import tester.testanalyzer.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:37 PM
 */

sealed abstract case class PathingStatus[+T <: StepData](stepData: T) extends ExecutionStatus

case class Continue[+T <: StepData](stepData: T) extends PathingStatus[T](stepData)
case class Success[+T <: StepData](stepData: T) extends PathingStatus[T](stepData)
case class Failure[+T <: StepData](stepData: T) extends PathingStatus[T](stepData)
