package utilitylib.typewarfare

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/30/12
 * Time: 1:17 PM
 */

object TypeWarfare {

  // Type disjunction, courtesy of the great Miles Sabin
  private type ~[X]     = X => Nothing
  private type |[X, Y]  = ~[~[X] with ~[Y]]
  private type ~~[X]    = ~[~[X]]
  type ||[X, Y] = { type T[L] = ~~[L] <:< (X | Y) }

}
