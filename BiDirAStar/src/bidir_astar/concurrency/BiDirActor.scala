package bidir_astar.concurrency

import actors.Actor
import bidir_astar.BiDirStepData
import astar_base.statuses.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:26 PM
 */

sealed abstract class BiDirActor[T <: BiDirStepData](es: ExecutionStatus[T], i: Int,
                                                     dFunc: (T, Int) => ExecutionStatus[T],
                                                     sFunc: T => T) extends Actor {

    val status = es
    val iters = i
    val decide = dFunc
    val step = sFunc

    protected def moveAndMutate() : (ExecutionStatus[T], Int) = {
        val neoIters = iters + 1
        val neoStatus = decide(step(status.stepData), neoIters)
        (neoStatus, neoIters)
    }
    
}

case class StartToGoal[T <: BiDirStepData](exeStatus: ExecutionStatus[T], itrs: Int,
                                           decideFunc: (T, Int) => ExecutionStatus[T], stepFunc: T => T)
                                           extends BiDirActor[T](exeStatus, itrs, decideFunc, stepFunc) {
    def act() {
        react {
            case "start" => {
                val (actStatus, actIters) = moveAndMutate()
                reply(new StartToGoal(actStatus, actIters, decide, step))
            }
        }

    }

}

case class GoalToStart[T <: BiDirStepData](exeStatus: ExecutionStatus[T], itrs: Int,
                                           decideFunc: (T, Int) => ExecutionStatus[T], stepFunc: T => T)
                                           extends BiDirActor[T](exeStatus, itrs, decideFunc, stepFunc) {

    def act() {
        react {
            case "start" => {
                val (actStatus, actIters) = moveAndMutate()
                reply(new GoalToStart(actStatus, actIters, decide, step))
            }
        }
    }

}