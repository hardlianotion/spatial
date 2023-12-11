package spatial.hex


type B1 = Short
case class Index private (h3: H3, time: B1)

object Index:
  def fromVelocity (velocity: Double): Index =
    ???

  def fromAspectRatio (spaceLvl: Byte, timeLvl: Byte): Index =
    ???


case class H3T1Tree [T] private [hex](address: Index, rootRes: Byte, leafRes: Byte, root: TreeNode [T])

/**
 * H3BinTree implementation shares the H3Tree structure, but packs the spare bit on each tree level.
 * So the implementation assigns a meaning to 7 on each byte and strips it out before querying H3 index for
 * geographical information.
 *
 * The resulting tree structure can partition, for example, global information over a day to a resolution of 3 seconds and
 * 10 centimetres.  Which is good enough to use as a starting point.
 */
object H3T1Tree:
  ???
