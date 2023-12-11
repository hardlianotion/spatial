package sim.hex

import org.scalatest.flatspec.AnyFlatSpec

import scala.Right
import scala.language.postfixOps

import org.scalatest.matchers.should


class H3TreeTests extends AnyFlatSpec with should.Matchers:
  val root = h3CreateCellIndex (13, 0, Map (13 -> 6))

  "A tree that is a leaf" should "have depth 0" in:
    val maybeTree = H3Tree.fromRoot (root, 0, Leaf (2))
    maybeTree.isRight should be (true)

    for
      tree <- maybeTree
    do
      H3Tree.depth (tree) should be (0)

  "A subtree of a tree of size N that is accessed at a depth n" should "have a depth of N - n" in:
    val maybeTree = H3Tree.empty [Int, Int] (0, 7, root)
    maybeTree.isRight should be (true)

    for
      tree <- maybeTree
      maybeSubtree5 = H3Tree.subNode (tree, root, 5)
    do
      maybeSubtree5.isDefined should be (true)
      H3Tree.depth (maybeSubtree5.orNull) should be (2)

  "A tree that is a node" should "have depth 1 greater than its children" in:
    val maybeTwoTree = H3Tree.empty [Int, Int] (0, 2, root)
    maybeTwoTree.isRight should be (true)

    for
      twoTree <- maybeTwoTree
    do
      val maybeOneRoot = H3Tree.subNode [Int] (twoTree, 0, 1)
      maybeOneRoot.isDefined should be (true)

      val maybeOneTree = H3Tree.fromRoot (root, 2, maybeOneRoot.orNull)
      maybeOneTree.isRight should be (true)

      val oneTree = maybeOneTree.toOption.orNull
      H3Tree.depth (twoTree) should be (2)
      H3Tree.depth (oneTree) should be (H3Tree.depth (twoTree) - 1)

  "H3Tree.get" should "retrieve only value assigned by H3Tree.set" in:
    val rootIdx = h3CreateCellIndex (15, 0, Map(13 -> 5))
    val updatedIdx = h3CreateCellIndex (15, 0, Map(13 -> 5, 14 -> 1, 15 -> 1))
    val siblingIdx = h3CreateCellIndex (15, 0, Map(13 -> 5, 14 -> 3, 15 -> 2))
    val maybeTree = H3Tree.empty [Int, Int] (13, 15, rootIdx)

    maybeTree.isRight should be (true)

    for
      tree <- maybeTree
      initial = H3Tree.get (tree, updatedIdx, 1)
      _ = H3Tree.set (tree, updatedIdx, 1, 1)
      updated = H3Tree.get (tree, updatedIdx, 1)
    do
      initial should be (Some (0))
      updated should be (Some (1))
      H3Tree.get (tree, updatedIdx, 0) should be (Some(0))
      H3Tree.get (tree, updatedIdx, 2) should be (Some(0))
      H3Tree.get (tree, siblingIdx, 1) should be (Some(0))


  case class Data (idx: H3, data: Int)

  // NOTE - updates are all res 15 indices (check out the highest level index entry)
  val updates =
    List (
      Data (h3CreateCellIndex (15, 28, Map(11 -> 3, 13 -> 1, 14 -> 3, 15 -> 2)), 1),
      Data (h3CreateCellIndex (15, 28, Map(11 -> 3, 13 -> 1, 14 -> 3, 15 -> 1)), 10),
      Data (h3CreateCellIndex (15, 28, Map(11 -> 3, 13 -> 1, 14 -> 2, 15 -> 0)), 100),
      Data (h3CreateCellIndex (15, 28,  Map(11 -> 3, 13 -> 1, 14 -> 1, 15 -> 1)), 1000),
    )
  val rootIdx = h3CreateCellIndex (15, 28, Map(11 -> 3, 13 -> 1))

  "A tree level" should "accumulate all values accumulate by its sub-trees" in:
    updates.forall (x => instance.isValidCell (x.idx)) should be (true)

    instance.isValidCell (rootIdx) should be (true)

    val maybeTree = H3Tree.empty [Int, Int] (12, 15, rootIdx)

    maybeTree.isRight should be (true)

    for
      tree <- maybeTree
      item <- updates
    do
      instance.isValidCell (item.idx) should be (true)
      H3Tree.get (tree, item.idx, 0) should be (Some (0))
      H3Tree.get (tree, item.idx, 1) should be (Some (0))
      H3Tree.get (tree, item.idx, 2) should be (Some (0))

    // let's update the tree and check it
    for
      tree <- maybeTree
      result <- updates.map (x => H3Tree.accumulate (tree, x.idx, x.data))
    do
      result should be (true)
      H3Tree.get (tree, updates (0).idx, 3) should be (Some (1))
      H3Tree.get (tree, updates (1).idx, 3) should be (Some (10))
      H3Tree.get (tree, updates (2).idx, 3) should be (Some (100))
      H3Tree.get (tree, updates (3).idx, 3) should be (Some (1000))
      H3Tree.get (tree, updates (0).idx, 2) should be (Some (11))
      H3Tree.get (tree, updates (1).idx, 2) should be (Some (11))
      H3Tree.get (tree, updates (2).idx, 2) should be (Some (100))
      H3Tree.get (tree, updates (3).idx, 2) should be (Some (1000))
      H3Tree.get (tree, updates (0).idx, 0) should be (Some (1111))
      H3Tree.get (tree, updates (3).idx, 0) should be (Some (1111))

  "A tree 'shifted' from another by n degrees" should "has node at level y corresponding to n + y in the lower tree" in:
    updates.forall (x => instance.isValidCell (x.idx)) should be (true)

    instance.isValidCell (rootIdx) should be (true)

    // NOTE lowerTree is shifted 2 'down' from higherTree
    val lowerMaybeTree = H3Tree.empty [Int, Int] (12, 15, rootIdx)
    val higherMaybeTree = H3Tree.empty [Int, Int] (10, 13, rootIdx)

    lowerMaybeTree.isRight should be (true)
    higherMaybeTree.isRight should be (true)

    for
      lowerTree <- lowerMaybeTree
      higherTree <- higherMaybeTree

      // NOTE higherTree collects data at two levels higher than lowerTree
      lowerResult = updates.map (x => H3Tree.accumulate (lowerTree, x.idx, x.data)).forall (x => x)
      higherResult = updates.map (x => H3Tree.accumulate (higherTree, x.idx, x.data)).forall (x => x)

      // NOTE so we collect from nodes two levels higher in lowerTree than higherTree
      lowerTest1 = H3Tree.get (lowerTree, updates (0).idx, 0)
      lowerTest2 = H3Tree.get (lowerTree, updates (3).idx, 0)

      higherTest1 = H3Tree.get (higherTree, updates (0).idx, 2)
      higherTest2 = H3Tree.get (higherTree, updates (3).idx, 2)
    do
      lowerResult should be (true)
      higherResult should be (true)

      // NOTE which means the results compared should be the same
      lowerTest1 should be (Some (1111))
      lowerTest2 should be (Some (1111))
      lowerTest1 should be (higherTest1)
      lowerTest2 should be (higherTest2)

  // NOTE - this is a res-15 index
  val anotherRootIdx = h3CreateCellIndex (15, 28, Map(11 -> 3, 12 -> 1, 15 -> 1))

  "A tree that cannot contain an index" should "not be able to hold the index data" in:
    instance.isValidCell (anotherRootIdx) should be (true)

    val maybeTree = H3Tree.empty [Int, Int] (12, 15, anotherRootIdx)

    maybeTree.isRight should be (true)
    // now none of the updates should be available in the tree
    for
      tree <- maybeTree
      result = updates.map (x => H3Tree.accumulate (tree, x.idx, x.data)).forall (x => x)
    do
      result should be (false)
      H3Tree.get (tree, updates (0).idx, 0).isDefined should be (false)
      H3Tree.get (tree, updates (1).idx, 0).isDefined should be (false)
      H3Tree.get (tree, updates (2).idx, 0).isDefined should be (false)
      H3Tree.get (tree, updates (3).idx, 0).isDefined should be (false)
      H3Tree.get (tree, updates (0).idx, 1).isDefined should be (false)
      H3Tree.get (tree, updates (1).idx, 1).isDefined should be (false)
      H3Tree.get (tree, updates (2).idx, 1).isDefined should be (false)
      H3Tree.get (tree, updates (3).idx, 1).isDefined should be (false)
      H3Tree.get (tree, updates (0).idx, 2).isDefined should be (false)
      H3Tree.get (tree, updates (3).idx, 2).isDefined should be (false)

  "An index with resolution < root resolution" should "not be stored in the tree" in:
    val maybeTree = H3Tree.empty [Int, Int] (12, 15, anotherRootIdx)

    maybeTree.isRight should be (true)

    val cannotInsert =
      // NOTE - this is a level 11 index - all indices >= 12 are masked out with 7.
      h3CreateCellIndex (11, 28, Map(11 -> 6))

    for
      tree <- maybeTree
      result = H3Tree.accumulate (tree, cannotInsert, 5)
    do
      result should be (false)
      H3Tree.get (tree, cannotInsert, 1).isDefined should be (false)

  "An index such that rootRes < resolution < leafRes" should "be stored in level (resolution - rootRes) in the tree" in:
    val maybeTree = H3Tree.empty [Int, Int] (12, 14, anotherRootIdx)

    maybeTree.isRight should be (true)

    // NOTE - this is a level 14 index - all indices >= 15 are masked out with 7.
    // NOTE - canInsert is clearly containable by maybeTree because anotherRootIx and canInsert have
    //        identical indices down to index 12, which is equal to maybeTree.rootRes
    val canInsert = h3CreateCellIndex (14, 28, Map(11 -> 3, 12 -> 1, 14 -> 3))

    instance.isValidCell (canInsert) should be (true)

    for
      tree <- maybeTree
      result = H3Tree.accumulate (tree, canInsert, 5)
    do
      result should be (true)
      H3Tree.get (tree, canInsert, 0) should be (Some (5))
      H3Tree.get (tree, canInsert, 1) should be (Some (5))
      H3Tree.get (tree, canInsert, 2) should be (Some (5))
      // NOTE get at the "natural" level also works.
      H3Tree.get (tree, canInsert) should be (Some (5))

  "Constructing trees with data" should "accumulate all values in its sub-trees" in:
    val maybeTree = H3Tree.fromData [Int, Int] (12, 15, rootIdx, updates.map (x => (x.idx, x.data)):_*)

    maybeTree.isRight should be (true)
    // let's check out the updated tree
    for
      tree <- maybeTree
    do
      H3Tree.get (tree, updates (0).idx, 3) should be (Some (1))
      H3Tree.get (tree, updates (1).idx, 3) should be (Some (10))
      H3Tree.get (tree, updates (2).idx, 3) should be (Some (100))
      H3Tree.get (tree, updates (3).idx, 3) should be (Some (1000))
      H3Tree.get (tree, updates (0).idx, 2) should be (Some (11))
      H3Tree.get (tree, updates (1).idx, 2) should be (Some (11))
      H3Tree.get (tree, updates (2).idx, 2) should be (Some (100))
      H3Tree.get (tree, updates (3).idx, 2) should be (Some (1000))
      H3Tree.get (tree, updates (0).idx, 0) should be (Some (1111))
      H3Tree.get (tree, updates (3).idx, 0) should be (Some (1111))
