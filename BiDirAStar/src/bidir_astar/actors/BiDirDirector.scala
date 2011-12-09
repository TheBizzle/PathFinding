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

class BiDirDirector[T <: BiDirStepData]
                   (forwardStepData: T, backwardsStepData: T, iters: Int, maxIters: Int,
                    decisionFunc: (T, Int, Int) => ExecutionStatus[T], stepFunc: T => T) extends Actor {

    val decide = decisionFunc
    val step = stepFunc

    var startGoalActor: Option[StartToGoal[T]] = Some(new StartToGoal[T](Continue(forwardStepData), iters, maxIters, decide, step))
    var goalStartActor: Option[GoalToStart[T]] = Some(new GoalToStart[T](Continue(backwardsStepData), iters, maxIters, decide, step))
    var isStarted = false

    def act() {

        startGoalActor.get.start()
        goalStartActor.get.start()

        loop {
            react {
                case "begin" => isStarted = true
                case x: StartToGoal[T] => {
                    if (isStarted) {
                        startGoalActor = Some(x)
                        goalStartActor match {
                            case None => // Must wait until both actors are ready
                            case Some(y) => takeAction(x, y)
                        }
                    }
                    else throw new InvalidParameterException("Message the director with \"begin\" before sending it messages!")
                }
                case x: GoalToStart[T] => {
                    if (isStarted) {
                        goalStartActor = Some(x)
                        startGoalActor match {
                            case None => // Must wait until both actors are ready
                            case Some(y) => takeAction(y, x)
                        }
                    }
                    else throw new InvalidParameterException("Message the director with \"begin\" before sending it messages!")
                }
                case _ => throw new InvalidParameterException
            }
        }

    }

    private def takeAction(stg: StartToGoal[T], gts: GoalToStart[T]) {
        stg.status match {
            case Failure(x)  => reply(Failure(x))
            case Success(x)  => reply(Success(x))
            case Continue(x) =>
                gts.status match {
                    case Failure(y)  => reply(Failure(y))
                    case Success(y)  => reply(Success(y))
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

    /**
     * Above, on success, we need to merge the breadcrumbs!
     */
    private def shareDataAndMutate(stg: StartToGoal[T], gts: GoalToStart[T]) : (StartToGoal[T], GoalToStart[T]) = {
        
        val stgStepData = stg.status.stepData
        val gtsStepData = gts.status.stepData

        // I find this very displeasing
        stgStepData.mergeShared(gtsStepData.loc, gtsStepData.breadcrumbArr)
        gtsStepData.mergeShared(stgStepData.loc, stgStepData.breadcrumbArr)

        val neoStg = new StartToGoal[T](Continue(stgStepData), stg.iters, stg.maxIters, stg.decide, stg.step)
        val neoGts = new GoalToStart[T](Continue(gtsStepData), gts.iters, gts.maxIters, gts.decide, gts.step)

        (neoStg, neoGts)
        
    }


}