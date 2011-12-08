package bidir_astar.actors

import actors.Actor

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 4:26 PM
 */

sealed abstract class BiDirActor extends Actor {
    def act() {
        
    }
}

case class StartToGoal extends BiDirActor
case class GoalToStart extends BiDirActor