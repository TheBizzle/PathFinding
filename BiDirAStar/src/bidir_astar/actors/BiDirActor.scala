package bidir_astar.actors

import actors.Actor
import bidir_astar.BiDirStepData
import astar_base.statuses.ExecutionStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:26 PM
 */

sealed abstract class BiDirActor[T <: BiDirStepData]
                                (es: ExecutionStatus[T], i: Int, maxI: Int,
                                 dFunc: (T, Int, Int) => ExecutionStatus[T], sFunc: T => T) extends Actor {

    val status = es
    val iters = i
    val maxIters = maxI
    val decide = dFunc
    val step = sFunc

    protected def moveAndMutate() : (ExecutionStatus[T], Int) = {
        val neoIters = iters + 1
        val neoStatus = decide(step(status.stepData), neoIters, maxIters)
        (neoStatus, neoIters)
    }
    
}

case class StartToGoal[T <: BiDirStepData]
                      (exeStatus: ExecutionStatus[T], itrs: Int, maxItrs: Int,
                       decideFunc: (T, Int, Int) => ExecutionStatus[T], stepFunc: T => T)
                       extends BiDirActor[T](exeStatus, itrs, maxItrs, decideFunc, stepFunc) {
    def act() {
        val (actStatus, actIters) = moveAndMutate()
        sender ! new StartToGoal(actStatus, actIters, maxIters, decide, step)
    }

}

case class GoalToStart[T <: BiDirStepData]
                      (exeStatus: ExecutionStatus[T], itrs: Int, maxItrs: Int,
                       decideFunc: (T, Int, Int) => ExecutionStatus[T], stepFunc: T => T)
                       extends BiDirActor[T](exeStatus, itrs, maxItrs, decideFunc, stepFunc) {

    def act() {
        val (actStatus, actIters) = moveAndMutate()
        sender ! new GoalToStart(actStatus, actIters, maxIters, decide, step)
    }

}