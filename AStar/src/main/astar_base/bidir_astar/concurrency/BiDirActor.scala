package astar_base.bidir_astar.concurrency

import actors.Actor
import astar_base.bidir_astar.BiDirStepData
import pathfinding.statuses.PathingStatus
import pathfinding.breadcrumb.Breadcrumb

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:26 PM
 */

sealed abstract class BiDirActor[T <: BiDirStepData](es: PathingStatus[T],
                                                     dFunc: T => PathingStatus[T],
                                                     sFunc: T => (T, List[Breadcrumb])) extends Actor {

    var status = es           // 6... 6, 6: The Number of the Bug
    val decide = dFunc
    val step = sFunc

    protected def moveAndMutate() : (PathingStatus[T], List[Breadcrumb]) = {
        val (neoStepData, neoCrumbs) = step(status.stepData)
        status = decide(neoStepData)
        (status, neoCrumbs)
    }

    def act() {
        loop {
            react {
                case (BiDirActor.AssimilateMessageStr, crumbList: List[Breadcrumb]) => status.stepData.assimilateBreadcrumbs(crumbList)
                case  BiDirActor.StartMessageStr                                    => reply(moveAndMutate())
                case  BiDirActor.StopMessageStr                                     => exit()
            }
        }
    }

}

private[concurrency] object BiDirActor {
    val StartMessageStr = "start"
    val AssimilateMessageStr = "assimilate"
    val StopMessageStr = "stop"
}

case class StartToGoal[T <: BiDirStepData](exeStatus: PathingStatus[T],
                                           decideFunc: T => PathingStatus[T],
                                           stepFunc: T => (T, List[Breadcrumb]))
                                           extends BiDirActor[T](exeStatus, decideFunc, stepFunc)

case class GoalToStart[T <: BiDirStepData](exeStatus: PathingStatus[T],
                                           decideFunc: T => PathingStatus[T],
                                           stepFunc: T => (T, List[Breadcrumb]))
                                           extends BiDirActor[T](exeStatus, decideFunc, stepFunc)
