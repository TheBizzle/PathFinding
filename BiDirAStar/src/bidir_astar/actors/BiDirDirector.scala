package bidir_astar.actors

import actors.Actor
import bidir_astar.BiDirStepData

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:19 PM
 */

class BiDirDirector(startToGoal: StartToGoal, goalToStart: GoalToStart,
                    iterationFunc: (BiDirStepData, Int, Int) => BiDirStepData,
                    stepFunc: BiDirStepData => BiDirStepData) extends Actor {

    var startGoalActor = startToGoal
    var goalStartActor = goalToStart

    val iterate = iterationFunc
    val step = stepFunc

    def act() {

        // first actor.start
        // second actor.start

        // iterate(step(BiDirStepData(freshLoc, stepData)), iters + 1, maxIters)
        
    }
    
}