package pathfinding.pathingmap.terrain

import datastructure.bihashmap.BiHashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 9:37 PM
 */

object TerrainCharConverter {

    private val conversionMap = BiHashMap(
        Ant -> 'a',
        Goal -> 'G',
        Empty -> '_',
        Food -> 'f',
        Mound -> 'O',
        Wall -> 'D',
        Water -> '%',
        Path -> 'x',
        Query -> '.',
        Self -> '*'
    )

    def apply(c: Char) : Option[Terrain] = {
        conversionMap.get(c)
    }

    def apply(t: Terrain) : Option[Char] = {
        conversionMap.get(t)
    }

}