package pathfinding.pathingmap.terrain

import datastructure.mutable.BiHashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/17/11
 * Time: 9:37 PM
 */

object TerrainCharConverter {

  def apply(c: Char)    = ConversionMap.get(c)
  def apply(t: Terrain) = ConversionMap.get(t)

  private val ConversionMap = BiHashMap(
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

}
