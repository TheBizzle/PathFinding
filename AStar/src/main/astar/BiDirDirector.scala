package astar

import
  scala.{ annotation, concurrent },
    annotation.tailrec,
    concurrent.{ Await, duration },
      duration._

import
  akka.{ actor, pattern, util },
    actor.{ Actor, ActorRef, ActorSystem, PoisonPill, Props },
    pattern.ask,
    util.Timeout

import
  pathfinding.{ coordinate, PathingStatus },
    coordinate.{ Breadcrumb, Coordinate2D },
    PathingStatus._

import
  base.UnexpectedDataException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:19 PM
 */

class BiDirDirector[T <: BiDirStepData](decisionFunc: T => PathingStatus[T], stepFunc: T => (T, Seq[Breadcrumb])) {

  private val actorSystem = ActorSystem("BiDir")

  val decide = decisionFunc
  val step   = stepFunc

  def direct(forwardStepData: T, backwardsStepData: T) : PathingStatus[T] = {
    val stg    = actorSystem.actorOf(Props(new BiDirActor(Continue(forwardStepData),   decide, step)), name = "stg")
    val gts    = actorSystem.actorOf(Props(new BiDirActor(Continue(backwardsStepData), decide, step)), name = "gts")
    val outVal = evaluateActions(stg, gts)
    terminateActors(stg, gts)
    outVal
  }

  @tailrec
  private def evaluateActions(stg: ActorRef, gts: ActorRef) : PathingStatus[T] = {

    val ((stgStatus, stgCrumbs), (gtsStatus, gtsCrumbs)) = runActionsForResult(stg, gts)

    stgStatus match {
      case result @ Failure(_) =>
        result
      case Success(x) =>
        mergeBreadcrumbsForForwardOnSuccess(x, gtsStatus.stepData)
      case Continue(_) =>
        gtsStatus match {
          case result @ Failure(_) =>
            result
          case Success(x) =>
            mergeBreadcrumbsForBackwardsOnSuccess(x, stgStatus.stepData)
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

  def terminateActors(actorArgs: ActorRef*) {
    actorArgs foreach (_ ! PoisonPill)
  }

  def runActionsForResult(stg: ActorRef, gts: ActorRef) : ((PathingStatus[T], Seq[Breadcrumb]), (PathingStatus[T], Seq[Breadcrumb])) = {

    import BiDirMessage._
    import concurrent.ExecutionContext.Implicits.global

    implicit val timeout = Timeout(1.second)

    val takeOneStep = (actor: ActorRef) => Await.result(actor ? Start, 1.second) match { case StepState(result: Result[T]) => result }

    (takeOneStep(stg), takeOneStep(gts))

  }

}

private class BiDirActor[T <: BiDirStepData](es: PathingStatus[T], dFunc: T => PathingStatus[T], sFunc: T => (T, Seq[Breadcrumb])) extends Actor {

  import BiDirMessage._

  var status = es
  val decide = dFunc
  val step   = sFunc

  protected def moveAndMutate() : Result[T] = {
    val (neoStepData, neoCrumbs) = step(status.stepData)
    status = decide(neoStepData)
    (status, neoCrumbs)
  }

  override def receive = {
    case Assimilate(crumbs) => status.stepData.assimilateBreadcrumbs(crumbs)
    case Start              => sender ! StepState(moveAndMutate())
  }

}

sealed trait BiDirMessage
object BiDirMessage {

  type BDSD              = BiDirStepData
  type Result[T <: BDSD] = (PathingStatus[T], Seq[Breadcrumb])

  case class  Assimilate(crumbs: Seq[Breadcrumb])     extends BiDirMessage
  case object Start                                   extends BiDirMessage
  case class  StepState[T <: BDSD](result: Result[T]) extends BiDirMessage

}

