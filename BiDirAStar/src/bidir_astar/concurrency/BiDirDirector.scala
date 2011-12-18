package bidir_astar.concurrency

import bidir_astar.BiDirStepData
import astar_base.statuses._
import coordinate.Coordinate
import java.security.InvalidParameterException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:19 PM
 */

class BiDirDirector[T <: BiDirStepData](decisionFunc: (T, Int) => ExecutionStatus[T], stepFunc: T => T) {

    val decide = decisionFunc
    val step = stepFunc

    def direct(forwardStepData: T, backwardsStepData: T) : ExecutionStatus[T] = {
        val stg = new StartToGoal[T](Continue(forwardStepData), 0, decide, step)
        val gts = new GoalToStart[T](Continue(backwardsStepData), 0, decide, step)
        evaluateActions(stg, gts)
    }

    private def evaluateActions(stg: StartToGoal[T], gts: GoalToStart[T]) : ExecutionStatus[T] = {

        val (resultStg, resultGts) = runActionsForResult(stg, gts)

        resultStg.status match {
            case (result @ Failure(_)) => result
            case Success(x) => mergeBreadcrumbsForForwardOnSuccess(x, resultGts.status.stepData)
            case Continue(_) =>
                resultGts.status match {
                    case (result @ Failure(_)) => result
                    case Success(x) => mergeBreadcrumbsForBackwardsOnSuccess(x, resultStg.status.stepData)
                    case Continue(_) => { val (neoStg, neoGts) = shareDataAndTransform(resultStg, resultGts); evaluateActions(neoStg, neoGts) }
                }
        }

    }

    def mergeBreadcrumbsForForwardOnSuccess(forwardData: T, backwardsData: T) : Success[T] = {
        backwardsData.reverseBreadcrumbs()
        Success(mergeBreadcrumbs(forwardData, backwardsData, forwardData.goal, forwardData.loc))
    }

    def mergeBreadcrumbsForBackwardsOnSuccess(backwardsData: T, forwardData : T) : Success[T] = {
        backwardsData.reverseBreadcrumbs()
        Success(mergeBreadcrumbs(backwardsData, forwardData, backwardsData.loc, backwardsData.goal))
    }

    def mergeBreadcrumbs(myData: T, thatData: T, startLoc: Coordinate, endLoc: Coordinate) : T = {

        val myCrumbs = myData.breadcrumbArr
        val thoseCrumbs = thatData.breadcrumbArr

        // DEBUGGING STATEMENTS
        //myCrumbs foreach ( x => print( x(0).toString + "||") ); print('\n')
        //thoseCrumbs foreach ( x => print( x(0).toString + "||") ); print("\n\n")

        var holder = startLoc

        do {
            if ((holder.x == Coordinate.InvalidValue) || (holder.y == Coordinate.InvalidValue)) throw new InvalidParameterException
            val crumb = thoseCrumbs(holder.x)(holder.y)
            myCrumbs(holder.x)(holder.y) = crumb
            holder = crumb
        } while (!(holder overlaps endLoc))

        // DEBUGGING STATEMENTS
        //myCrumbs foreach ( x => print( x(0).toString + "||") ); print('\n')
        //thoseCrumbs foreach ( x => print( x(0).toString + "||") ); print('\n')

        myData

    }

    def runActionsForResult(stg: StartToGoal[T], gts: GoalToStart[T]) : (StartToGoal[T], GoalToStart[T]) = {

        stg.start()
        val stgFuture = (stg !! "start")

        gts.start()
        val gtsFuture = (gts !! "start")

        val stgVal = stgFuture().asInstanceOf[StartToGoal[T]]
        val gtsVal = gtsFuture().asInstanceOf[GoalToStart[T]]

        (stgVal, gtsVal)

    }

    private def shareDataAndTransform(stg: StartToGoal[T], gts: GoalToStart[T]) : (StartToGoal[T], GoalToStart[T]) = {
        
        val stgStepData = stg.status.stepData
        val gtsStepData = gts.status.stepData

        /**
         * This can be fixed at some point to get rid of othersBreadcrumbsArr as a var in BiDirStepData
         * This can be done with A going in B's breadcrumbs, finding the breadcrumb (C) of B.loc,
         * and then updating A.othersBreadcrumbsArr for all neighbors of C
         *
         * At least... I THINK that will work.  I'd rather get this whole thing working before I try that and make things worse for myself, though.
         */
        // I find this very displeasing
        stgStepData.assimilateBreadcrumbs(gtsStepData.breadcrumbArr)
        gtsStepData.assimilateBreadcrumbs(stgStepData.breadcrumbArr)

        // Actually, I find this whole function displeasing
        val neoStg = new StartToGoal[T](Continue(stgStepData), stg.iters, stg.decide, stg.step)
        val neoGts = new GoalToStart[T](Continue(gtsStepData), gts.iters, gts.decide, gts.step)

        (neoStg, neoGts)
        
    }


}