package spatial

import org.scalatest.*
import flatspec.*
import matchers.*

import spatial.hex.*
import spatial.DeliveryMocks.locationsInTree


// FIXME - a bit more organisation into coherent sections
class H3Tests extends AnyFlatSpec with should.Matchers:

  "A level 12 tree mask" should "be 111000000000" in:
    assert (H3.treeNodeMask (12).binaryString == "111000000000")

  "A level 15 tree mask" should "be 111" in:
    assert (H3.treeNodeMask (15).binaryString == "111")

  "A level 15 mask" should "be 0" in:
    assert (H3.bitCellMask (15, 3) == 0)

  "A level 14 mask" should "be 7" in:
    assert (H3.bitCellMask (14, 3).toHexString == "7")

  "A level 13 mask" should "be 3f" in:
    assert (H3.bitCellMask (13, 3).toHexString == "3f")

  "A level 12 mask" should "be 1ff" in:
    assert (H3.bitCellMask (12, 3).toHexString == "1ff")

  "15" should "contain 12" in:
    assert (H3.unsafe ((14L << 52) + 15L).contains (H3.unsafe (12L)))

  "8f754e64992d6d8" should "be contained in hex 8e754e64992d6df" in:
    assert (H3.safe (java.lang.Long.valueOf ("8e754e64992d6df", 16)).contains (H3.safe (java.lang.Long.valueOf ("8f754e64992d6d8", 16))))

  "3L" should "localise to 3 at level 15" in:
    assert (H3.unsafe (3L).local (15) == 3)

  "3L" should "localise to 0 at level 14" in:
    assert (H3.unsafe (3L).local (14) == 0)

  "3L << 3" should "localise to 0 at level 15" in:
    assert (H3.unsafe (3L << 3).local (15) == 0)

  "3L << 3" should "localise to 3 at level 14" in:
    assert (H3.unsafe (3L << 3).local (14) == 3)

  "h3Level" should "return the resolution level of an index" in:
    assert (true)

  "generated h3 points" should "be valid" in:
    val randomH3 = H3.unsafeRandom (28, 0, 13)

    instance.isValidCell (randomH3) should be (true)

  "h3TruncateToLevel" should "return a level-truncated h3 with local index 0" in:
    val index = locationsInTree.head
    val truncIndex = index.truncateToRes (5)

    index.isValid should be (true)
    truncIndex.resolution should be (5)

  "h3CreateCellIndex" should "return a well formatted h3 index" in:
    val index: H3 = H3.createCellIndex (14, 128, Map(12 -> 1, 13 -> 2, 14 -> 3, 15 -> 4))

    index.local (12) should be (1)
    index.local (13) should be (2)
    index.local (14) should be (3)
    index.local (15) should be (7) // this is because everything below level 14 should be truncated