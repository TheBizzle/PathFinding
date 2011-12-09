package bidir_astar.actors

import actors.Actor
import bidir_astar.BiDirStepData
import astar_base.statuses._
import java.security.InvalidParameterException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:19 PM
 */

class BiDirDirector[+T <: BiDirStepData](stepData: T, iters: Int, maxIters: Int,
                                         decisionFunc: (T, Int, Int) => ExecutionStatus[T],
                                         stepFunc: T => T) extends Actor {

    val decide = decisionFunc
    val step = stepFunc
    
    var startGoalActor: Option[StartToGoal[T]] = Some(new StartToGoal(Continue(stepData.clone()), iters, maxIters, decide, step))
    var goalStartActor: Option[GoalToStart[T]] = Some(new GoalToStart(Continue(stepData.cloneForBiBackwards()), iters, maxIters, decide, step))

    def act() {

        startGoalActor.get.start()
        goalStartActor.get.start()

        loop {
            react {
                case x: StartToGoal[T] => {
                    startGoalActor = Some(x)
                    goalStartActor match {
                        case None => // Must wait until both actors are ready
                        case Some(y) => takeAction(x, y)
                    }
                }
                case x: GoalToStart[T] => {
                    goalStartActor = Some(x)
                    startGoalActor match {
                        case None => // Must wait until both actors are ready
                        case Some(y) => takeAction(y, x)
                    }
                }
                case _ => throw new InvalidParameterException
            }
        }

    }

    private def takeAction(stg: StartToGoal[T], gts: GoalToStart[T]) {
        stg.status match {
            case Failure(x)  => sender ! Failure(x)
            case Success(x)  => sender ! Success(x)
            case Continue(x) =>
                gts.status match {
                    case Failure(y)  => sender ! Failure(y)
                    case Success(y)  => sender ! Success(y)
                    case Continue(y) => continueSearch(stg, gts)
                }
        }
    }


    private def continueSearch(stg: StartToGoal[T], gts: GoalToStart[T]) {

        startGoalActor = None
        goalStartActor = None

        val (startActor, endActor) = shareDataAndMutate(stg, gts)

        startActor.start()
        endActor.start()

    }

    private def shareDataAndMutate(stg: StartToGoal[T], gts: GoalToStart[T]) : (StartToGoal[T], GoalToStart[T]) = {
        
        val stgStepData = stg.status.stepData
        val gtsStepData = gts.status.stepData

        val neoStgStepData = BiDirStepData.importShared(stgStepData, gtsStepData.loc, gtsStepData.breadcrumbArr)
        val neoGtsStepData = BiDirStepData.importShared(gtsStepData, stgStepData.loc, stgStepData.breadcrumbArr)

        val neoStg = new StartToGoal[T](Continue(neoStgStepData), stg.iters, stg.maxIters, stg.decide, stg.step)
        val neoGts = new GoalToStart[T](Continue(neoGtsStepData), gts.iters, gts.maxIters, gts.decide, gts.step)

        (neoStg, neoGts)
        
    }


}