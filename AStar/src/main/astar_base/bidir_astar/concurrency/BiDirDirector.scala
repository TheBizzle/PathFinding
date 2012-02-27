package astar_base.bidir_astar.concurrency

import astar_base.bidir_astar.BiDirStepData
import pathfinding.statuses._
import pathfinding.coordinate.Coordinate
import annotation.tailrec
import astar_base.exceptions.UnexpectedDataException
import pathfinding.breadcrumb.Breadcrumb

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:19 PM
 */

class BiDirDirector[T <: BiDirStepData](decisionFunc: T => PathingStatus[T], stepFunc: T => (T, List[Breadcrumb])) {

    val decide = decisionFunc
    val step = stepFunc

    def direct(forwardStepData: T, backwardsStepData: T) : PathingStatus[T] = {
        val stg = new StartToGoal[T](Continue(forwardStepData), decide, step)
        val gts = new GoalToStart[T](Continue(backwardsStepData), decide, step)
        val outVal = evaluateActions(stg, gts)
        terminateActors(stg, gts)
        outVal
    }

    @tailrec
    private def evaluateActions(stg: StartToGoal[T], gts: GoalToStart[T]) : PathingStatus[T] = {

        val ((stgStatus, stgCrumbs), (gtsStatus, gtsCrumbs)) = runActionsForResult(stg, gts)

        stgStatus match {
            case (result @ Failure(_)) => result
            case Success(x) => mergeBreadcrumbsForForwardOnSuccess(x, gtsStatus.stepData)
            case Continue(_) =>
                gtsStatus match {
                    case (result @ Failure(_)) => result
                    case Success(x) => mergeBreadcrumbsForBackwardsOnSuccess(x, stgStatus.stepData)
                    case Continue(_) =>
                        stg ! (BiDirActor.AssimilateMessageStr, gtsCrumbs)
                        gts ! (BiDirActor.AssimilateMessageStr, stgCrumbs)
                        evaluateActions(stg, gts)
                }
        }

    }

    def mergeBreadcrumbsForForwardOnSuccess(forwardData: T, backwardsData: T) : Success[T] = {
        Success(mergeBreadcrumbs(forwardData, backwardsData, forwardData.loc, forwardData.goal, true))
    }

    def mergeBreadcrumbsForBackwardsOnSuccess(backwardsData: T, forwardData : T) : Success[T] = {
        backwardsData.reverseBreadcrumbs()
        Success(mergeBreadcrumbs(backwardsData, forwardData, backwardsData.loc, backwardsData.goal, false))
    }

    def mergeBreadcrumbs(myData: T, thatData: T, startLoc: Coordinate, endLoc: Coordinate, isForwards: Boolean) : T = {
        @tailrec def mergeHelper(holder: Coordinate, myCrumbs: Array[Array[Coordinate]], thoseCrumbs: Array[Array[Coordinate]]) {
            holder match {
                case Coordinate(Coordinate.InvalidValue, Coordinate.InvalidValue) => throw new UnexpectedDataException(holder.toString)
                case _ =>
                    val crumb = thoseCrumbs(holder.x)(holder.y)
                    val (indexer, updater) = if (isForwards) (crumb, holder) else (holder, crumb)
                    myCrumbs(indexer.x)(indexer.y) = updater
                    if (!(crumb overlaps endLoc)) mergeHelper(crumb, myCrumbs, thoseCrumbs)
            }
        }
        mergeHelper(startLoc, myData.breadcrumbArr, thatData.breadcrumbArr)
        myData
    }

    def terminateActors(actorArgs: BiDirActor[T]*) {
        actorArgs foreach ( _ ! BiDirActor.StopMessageStr )
    }

    def runActionsForResult(stg: StartToGoal[T], gts: GoalToStart[T]) : ((PathingStatus[T], List[Breadcrumb]), (PathingStatus[T], List[Breadcrumb])) = {

        stg.start()
        val stgFuture = (stg !! BiDirActor.StartMessageStr)

        gts.start()
        val gtsFuture = (gts !! BiDirActor.StartMessageStr)

        val stgTuple = stgFuture().asInstanceOf[(PathingStatus[T], List[Breadcrumb])]
        val gtsTuple = gtsFuture().asInstanceOf[(PathingStatus[T], List[Breadcrumb])]

        (stgTuple, gtsTuple)

    }

}
