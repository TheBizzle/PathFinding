package pathfinding.statuses

import pathfinding.StepData

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:37 PM
 */

sealed abstract class ExecutionStatus[+T <: StepData](data: T) {
    val stepData = data
}

case class Continue[+T <: StepData](sd: T) extends ExecutionStatus[T](sd)
case class Success[+T <: StepData](sd: T) extends ExecutionStatus[T](sd)
case class Failure[+T <: StepData](sd: T) extends ExecutionStatus[T](sd)
