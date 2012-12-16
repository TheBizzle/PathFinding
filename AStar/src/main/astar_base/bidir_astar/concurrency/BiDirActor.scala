package astar_base.bidir_astar.concurrency

import actors.Actor
import astar_base.bidir_astar.BiDirStepData
import pathfinding.coordinate.Breadcrumb
import pathfinding.PathingStatus

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:26 PM
 */

sealed abstract class BiDirActor[T <: BiDirStepData](es: PathingStatus[T],
                                                     dFunc: T => PathingStatus[T],
                                                     sFunc: T => (T, Seq[Breadcrumb])) extends Actor {

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

private[concurrency] object BiDirMessage {
  case class  Assimilate(crumbs: Seq[Breadcrumb]) extends BiDirMessage
  case object Start                               extends BiDirMessage
  case object Stop                                extends BiDirMessage
}

case class StartToGoal[T <: BiDirStepData](exeStatus:  PathingStatus[T],
                                           decideFunc: T => PathingStatus[T],
                                           stepFunc:   T => (T, Seq[Breadcrumb]))
                                           extends BiDirActor[T](exeStatus, decideFunc, stepFunc)

case class GoalToStart[T <: BiDirStepData](exeStatus:  PathingStatus[T],
                                           decideFunc: T => PathingStatus[T],
                                           stepFunc:   T => (T, Seq[Breadcrumb]))
                                           extends BiDirActor[T](exeStatus, decideFunc, stepFunc)
