package astar

import actors.Actor
import annotation.tailrec

import pathfinding.{ coordinate, PathingStatus }
import coordinate.{ Breadcrumb, Coordinate2D }
import PathingStatus._

import base.UnexpectedDataException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:19 PM
 */

class BiDirDirector[T <: BiDirStepData](decisionFunc: T => PathingStatus[T], stepFunc: T => (T, Seq[Breadcrumb])) {

  val decide = decisionFunc
  val step   = stepFunc

  def direct(forwardStepData: T, backwardsStepData: T) : PathingStatus[T] = {
    val stg    = new StartToGoal[T](Continue(forwardStepData),   decide, step)
    val gts    = new GoalToStart[T](Continue(backwardsStepData), decide, step)
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
            stg ! (BiDirMessage.Assimilate(gtsCrumbs))
            gts ! (BiDirMessage.Assimilate(stgCrumbs))
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

  def mergeBreadcrumbs(myData: T, thatData: T, startLoc: Coordinate2D, endLoc: Coordinate2D, isForwards: Boolean) : T = {
    @tailrec def mergeHelper(holder: Coordinate2D, myCrumbs: Array[Array[Coordinate2D]], thoseCrumbs: Array[Array[Coordinate2D]]) {
      if (holder.isValid) {
        val crumb = thoseCrumbs(holder.x)(holder.y)
        val (indexer, updater) = if (isForwards) (crumb, holder) else (holder, crumb)
        myCrumbs(indexer.x)(indexer.y) = updater
        if (!crumb.overlaps(endLoc)) mergeHelper(crumb, myCrumbs, thoseCrumbs)
      }
      else
        throw new UnexpectedDataException(holder.toString)
    }
    mergeHelper(startLoc, myData.breadcrumbArr, thatData.breadcrumbArr)
    myData
  }

  def terminateActors(actorArgs: BiDirActor[T]*) {
    actorArgs foreach (_ ! BiDirMessage.Stop)
  }

  def runActionsForResult(stg: StartToGoal[T], gts: GoalToStart[T]) : ((PathingStatus[T], Seq[Breadcrumb]), (PathingStatus[T], Seq[Breadcrumb])) = {

    stg.start()
    val stgFuture = (stg !! BiDirMessage.Start)

    gts.start()
    val gtsFuture = (gts !! BiDirMessage.Start)

    val stgTuple = stgFuture().asInstanceOf[(PathingStatus[T], Seq[Breadcrumb])]
    val gtsTuple = gtsFuture().asInstanceOf[(PathingStatus[T], Seq[Breadcrumb])]

    (stgTuple, gtsTuple)

  }

}

sealed abstract class BiDirActor[T <: BiDirStepData](es: PathingStatus[T], dFunc: T => PathingStatus[T], sFunc: T => (T, Seq[Breadcrumb])) extends Actor {

  var status = es
  val decide = dFunc
  val step   = sFunc

  protected def moveAndMutate() : (PathingStatus[T], Seq[Breadcrumb]) = {
    val (neoStepData, neoCrumbs) = step(status.stepData)
    status = decide(neoStepData)
    (status, neoCrumbs)
  }

  def act() {
    import BiDirMessage._
    loop {
      react {
        case Assimilate(crumbs) => status.stepData.assimilateBreadcrumbs(crumbs)
        case Start => reply(moveAndMutate())
        case Stop  => exit()
      }
    }
  }

}

sealed trait BiDirMessage

object BiDirMessage {
  case class  Assimilate(crumbs: Seq[Breadcrumb]) extends BiDirMessage
  case object Start                               extends BiDirMessage
  case object Stop                                extends BiDirMessage
}

private case class StartToGoal[T <: BiDirStepData](exeStatus:  PathingStatus[T],
                                           decideFunc: T => PathingStatus[T],
                                           stepFunc:   T => (T, Seq[Breadcrumb]))
  extends BiDirActor[T](exeStatus, decideFunc, stepFunc)

private case class GoalToStart[T <: BiDirStepData](exeStatus:  PathingStatus[T],
                                           decideFunc: T => PathingStatus[T],
                                           stepFunc:   T => (T, Seq[Breadcrumb]))
  extends BiDirActor[T](exeStatus, decideFunc, stepFunc)
