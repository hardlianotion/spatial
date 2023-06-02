package sim.hex

import scala.annotation.tailrec

import sim.math.Aggregable
import sim.hex.contains as h3Contains


sealed trait TreeNode [T]:
  def setData (data: T): Unit
  def data: T

case class Leaf [T] (var data: T) extends TreeNode [T]:
  def setData (data: T): Unit =
    this.data = data

case class Node [T] private [hex] (var data: T, children: Array [TreeNode [T]]) extends TreeNode [T]:
  def setData (data: T): Unit =
    this.data = data

object Node:
  def unapply [T] (node: Node [T]): Some [(T, Array [TreeNode [T]])] =
    Some ((node.data, node.children))

/**
 * H3Tree - a hex-tree used for aggregating summary statistics for location data
 *
 * @param address - root node address
 * @param rootRes - hex resolution of root node
 * @param leafRes - hex resolution of leaf node
 * @param root - root node structure
 * @tparam T - node data type
 */
case class H3Tree [T] private [hex] (address: H3, rootRes: Int, leafRes: Int, root: TreeNode [T]):
  type NodeType = TreeNode [T]

  def depth: Int  =
    leafRes - rootRes

sealed trait H3TreeError extends RuntimeException

object H3TreeError:
//  case class AccessError (index: H3, level: Int, depth: Int) extends H3TreeError
//  case class SpanError (root: H3, index: H3) extends H3TreeError
  case class BadAddressError (address: H3) extends H3TreeError
  case class CreationError (rootRes: Int, leafRes: Int, indexRes: Int) extends H3TreeError
  case class AggregationError [A, T] (tree: H3Tree [A], data: Seq[(H3, T)]) extends H3TreeError

object H3Tree:
  val maxTreeDepth = 15
  val minTreeDepth = 0

  /**
   * contains - checks if index is addressable by tree
   * @param tree - a valid H3Tree
   * @param index - a valid H3 index
   * @tparam T - tree node data type
   * @return - true if tree contains index, false otherwise
   */
  def contains [T] (tree: H3Tree [T], index: H3): Boolean =
    h3Contains (h3TruncateToRes (tree.address, tree.rootRes), index)

  /**
   * returns the level of address relative to the root resolution
   * @param address - a valid H3 address
   */
  def level [T] (tree: H3Tree [T], address: H3): Int =
    h3Resolution (address) - tree.rootRes

  /**
   * depth - the length of the address path from node to leaf node.
   *         Prefer to use the H3Tree variant instead when calculating root
   *         node depth.
   * @param node - TreeNode
   * @tparam T - tree node data type
   * @return - node depth
   */
  def depth [T] (node: TreeNode [T]): Int =
    def impl (node: TreeNode [T]): Int =
      node match
        case Leaf (_) => 0
        case Node (_, cs) =>  1 + impl (cs (0))

    impl (node)

  /**
   * depth - the length of the address path from root to leaf node.
   * @param tree - H3Tree
   * @tparam T - tree node data type
   * @return - tree depth
   */
  def depth [T] (tree: H3Tree [T]): Int =
    assert (depth (tree.root) == tree.depth)
    depth (tree.root)

  /**
   * Constructor - Uses leaf node resolution and root node to site tree.
   *
   * @param address - root address
   * @param leafRes - hex resolution of leaf index
   * @param root - constructed root node
   * @tparam T - tree node data type
   * @return - H3Tree of depth h3Level (address) - H3Tree.depth (root)
   */
  def fromRoot [T] (address: H3, leafRes: Int, root: TreeNode [T]): Either [H3TreeError, H3Tree [T]] =
    val rootRes = leafRes - H3Tree.depth (root)
    // NOTE address resolution must be greater than leaf level in order for it to represent root address in the tree.
    if rootRes < 0 || leafRes < 0 || leafRes > h3Resolution (address) then
      Left (H3TreeError.CreationError (rootRes, leafRes, h3Resolution (address)))
    else if !instance.isValidCell (address) then
      Left (H3TreeError.BadAddressError (address))
    else
      Right (H3Tree[T] (address, leafRes - H3Tree.depth (root), leafRes, root))

  def unapply [T] (tree: H3Tree [T]): Some [(H3, Int, Int, TreeNode [T])] =
    Some ((tree.address, tree.rootRes, tree.leafRes, tree.root))

  /**
   * Constructor - builds empty tree with specified dimensions.
   *
   * @param rootRes - root hex resolution
   * @param leafRes - leaf hex resolution
   * @param address - root hex address
   * @param agg - appropriate implicit Aggregate instance must be in scope
   * @tparam A - tree node data type
   * @tparam T - increment type used to update tree node data
   * @return - constructed, empty tree.
   */
  def empty [A, T] (rootRes: Int, leafRes: Int, address: H3) (using agg: Aggregable [A, T]): Either [H3TreeError, H3Tree [A]] =
    // NOTE address resolution must be greater than leaf level in order for it to represent root address in the tree.
    if rootRes < 0 || leafRes < 0  || leafRes > h3Resolution (address) then
      Left (H3TreeError.CreationError (rootRes, leafRes, h3Resolution (address)))
    else if !instance.isValidCell (address) then
      Left (H3TreeError.BadAddressError (address))
    else
      def impl (depth: Int): TreeNode [A] =
        depth match
          case 0 => Leaf [A] (agg.empty)
          case n => Node [A] (agg.empty, (0 to 6).map (_ => impl (n - 1)).toArray)

      val depth = leafRes - rootRes
      val root = impl (depth)
      Right (H3Tree[A] (address, rootRes, leafRes, root))

  /**
   * Constructor - build-and-aggregate
   *
   * @param rootRes - root hex resolution
   * @param leafRes - leaf hex resolution
   * @param address - root node address
   * @param data - data to be added to the tree
   * @param agg - appropriate implicit Aggregate instance must be in scope
   * @tparam A - tree node data type
   * @tparam T - increment type used to update tree node data
   * @return - error or completed tree
   */
  def fromData [A, T] (rootRes: Int, leafRes: Int, address: H3, data: (H3, T)*) (using agg: Aggregable [A, T]): Either [H3TreeError, H3Tree [A]] =
    for
      tree <- empty [A, T] (rootRes, leafRes, address)
      success = data.forall (datum => H3Tree.accumulate [A, T] (tree, datum._1, datum._2))
      result <- if success then Right (tree) else Left (H3TreeError.AggregationError [A, T] (tree, data))
    yield
      result

  /**
   * subNode is a request for the sub-node of the tree root <level> down from <tree.root>,
   * that contains <index>.
   *
   * It returns None if such a sub-tree cannot be found, and Some (subNode) otherwise.
   * @param tree - tree from which a subtree is sought.
   * @param index - index of a point within the sub-node requested.
   * @param level - number of levels down from root node.
   * @tparam T - tree node data type
   * @return - the sub-node represented by index at level
   */
  private [hex] def subNode [T] (tree: H3Tree [T], index: H3, level: Int): Option [TreeNode [T]] =
    val resolution = tree.rootRes + level
    if contains (tree, index) && resolution >= 0 && resolution <= h3MaxRes then
      val node = processNodesToRes [T] (_ => ()) (tree.root, tree.rootRes, index, resolution)
      Some (node)
    else
      None

  /**
   * processNodesToRes - tracks down a tree branch determined by <index>, from <currentNode> at <currentRes> up to <targetRes>, applying
   *  a potentially mutating <process> at each node in the branch.
   *
   * @param currentNode - starting node to traverse the tree.
   * @param currentRes - the starting node resolution.
   * @param index - index of a point within the sub-node requested.
   * @param targetRes - target resolution to process in the tree, root = 1, lowest leaf = 15
   * @param process - a callback that processes visited nodes in order from starting node to searched node
   * @tparam T - tree node data type
   * @return - the sub-node represented by index at level
   */
  @tailrec
  private def processNodesToRes [T] (process: TreeNode[T] => Unit) (currentNode: TreeNode[T], currentRes: Int, index: H3, targetRes: Int): TreeNode[T] =
    currentNode match
      case node @ Node (data, children) =>
        process (node)
        if targetRes != currentRes then
          val childLocalIndex = localIndex (index, currentRes + 1)
          //println(s"${h3ExplainAddress(index)}, child local $childLocalIndex.  current res = $currentRes, target = $targetRes")
          processNodesToRes (process) (children (childLocalIndex), currentRes + 1, index, targetRes)
        else
          node
      case leaf @ Leaf (data) =>
        process (leaf)
        leaf

  /**
   * get - retrieves data corresponding to <level> in the tree.
   * @param tree - tree to query
   * @param index - index to get value
   * @param level - number of levels down from root node.
   * @tparam T - tree node data type
   * @return - data at index
   */

  def get [T] (tree: H3Tree [T], index: H3, level: Int): Option [T] =
    subNode (tree, index, level).map (_.data)

  /**
   * get - this version uses the index's own level to query the tree.
   * @param tree - tree to query
   * @param index - index to get value
   * @tparam T - tree node data type
   * @return - data at index
   */
  def get [T] (tree: H3Tree [T], index: H3): Option [T] =
    subNode (tree, index, level (tree, index)).map (_.data)

  /**
   * @param tree - tree to update
   * @param index - index whose value shall be set
   * @param level - number of levels down from root node.
   * @param data - data to set
   * @tparam T - tree node data type
   * @return - operation success (true) or failure (false)
   */
  def set [T] (tree: H3Tree [T], index: H3, level: Int, data: T): Boolean =
    subNode (tree, index, level)
      .fold (false) {
        x => x.setData(data)
        true
      }


  /**
   * get - this version uses the index's own level to mutate the tree.
   * @param tree - tree to update
   * @param index - index whose value shall be set
   * @param data - data to set
   * @tparam T - tree node data type
   * @return - operation success (true) or failure (false)
   */
  def set [T] (tree: H3Tree [T], index: H3, data: T): Boolean =
    set (tree, index, level (tree, index), data)

  /**
   * accumulate - A hex-tree is normally used to aggregate summary information from sub-nodes into each node.
   *  accumulate ensures that when an item is added to a tree leaf, that information is propagated up the
   *  higher-level nodes in the correct fashion.
   *
   * @param tree - hex-tree that stores summary information
   * @param index - address that data will be stored at
   * @param level - number of levels down from root node.
   * @param data - data to be stored in the tree.
   * @tparam T - tree node data type
   * @return - success (true) or failure (false) of the operation.
   */
  def accumulate [A, T] (tree: H3Tree [A], index: H3, level: Int, data: T) (using agg: Aggregable [A, T]): Boolean =
    val resolution = tree.rootRes + level
    if contains (tree, index) && resolution >= tree.leafRes && resolution <= h3MaxRes then
      processNodesToRes [A] (node => node.setData (agg.add (node.data, data))) (tree.root, tree.rootRes, index, resolution)
      true
    else
      false

  /**
   * accumulate - A hex-tree is normally used to aggregate summary information from sub-nodes into each node.
   *  accumulate ensures that when an item is added to a tree leaf, that information is propagated up the
   *  higher-level nodes in the correct fashion.
   *
   * @param tree - hex-tree that stores summary information
   * @param index - address that data will be stored at.
   * @param data - data to be stored in the tree at leaf level.
   * @tparam T - tree node data type
   * @return - success (true) or failure (false) of the operation.
   */
  def accumulate [A, T] (tree: H3Tree [A], index: H3, data: T) (using agg: Aggregable [A, T]): Boolean =
    accumulate (tree, index, level (tree, index), data)
