package astar_base.heuristic

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 12/8/11
 * Time: 1:38 PM
 */

object HeuristicLib {
    def manhattanDistance(h: HeuristicBundle) : Int = {
        math.abs(h.start.x - h.end.x) + math.abs(h.start.y - h.end.y)
    }
}