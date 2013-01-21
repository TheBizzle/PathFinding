package pathfinding.pathingmap

import
  datastructure.mutable.BiHashMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/4/11
 * Time: 12:22 AM
 */

sealed abstract class Terrain(val isPassable: Boolean)

object Terrain {

  def apply(c: Char)    = ConversionMap.get(c)
  def apply(t: Terrain) = ConversionMap.get(t)

  object Ant     extends Terrain(true)
  object Empty   extends Terrain(true)
  object Food    extends Terrain(true)
  object Goal    extends Terrain(true)
  object Mound   extends Terrain(true)
  object Invalid extends Terrain(false)
  object Path    extends Terrain(false)
  object Query   extends Terrain(false)
  object Self    extends Terrain(false)
  object Wall    extends Terrain(false)
  object Water   extends Terrain(false)

  private val ConversionMap = BiHashMap(
    Ant   -> 'a',
    Goal  -> 'G',
    Empty -> '_',
    Food  -> 'f',
    Mound -> 'O',
    Wall  -> 'D',
    Water -> '%',
    Path  -> 'x',
    Query -> '.',
    Self  -> '*'
  )

}
