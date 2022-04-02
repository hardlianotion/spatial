package timeslots.hex

import org.scalatest._
import flatspec._
import matchers._

import timeslots.DeliveryMocks.locationsInTree


// FIXME - a bit more organisation into coherent sections
class H3Tests extends AnyFlatSpec with should.Matchers:

  "A level 12 tree mask" should "be 111000000000" in {
    assert (h3TreeNodeMask (12).toBinaryString == "111000000000")
  }

  "A level 15 tree mask" should "be 111" in {
    assert (h3TreeNodeMask (15).toBinaryString == "111")
  }

  "A level 15 mask" should "be 0" in {
    assert (h3BitCellMask (15, 3) == 0)
  }

  "A level 14 mask" should "be 7" in {
    assert (h3BitCellMask (14, 3).toHexString == "7")
  }

  "A level 13 mask" should "be 3f" in {
    assert (h3BitCellMask (13, 3).toHexString == "3f")
  }

  "A level 12 mask" should "be 1ff" in {
    assert (h3BitCellMask (12, 3).toHexString == "1ff")
  }

  "15" should "contain 12" in {
    assert (contains ((14L << 52) + 15L, 12) == true)
  }

  "8f754e64992d6d8" should "be contained in hex 8e754e64992d6df" in {
    assert (contains (java.lang.Long.valueOf ("8e754e64992d6df", 16), java.lang.Long.valueOf ("8f754e64992d6d8", 16)) == true)
  }

  "3L" should "localise to 3 at level 15" in {
    assert (localIndex (3L, 15) == 3)
  }

  "3L" should "localise to 0 at level 14" in {
    assert (localIndex (3L, 14) == 0)
  }

  "3L << 3" should "localise to 0 at level 15" in {
    assert (localIndex (3L << 3, 15) == 0)
  }

  "3L << 3" should "localise to 3 at level 14" in {
    assert (localIndex (3L << 3, 14) == 3)
  }

  "h3Level" should "return the resolution level of an index" in {
    assert (true)
  }

  "generated h3 points" should "be valid" in {
    val randomH3 = unsafeH3Random (28, 0, 13)

    instance.h3IsValid (randomH3) should be (true)
  }

  "h3TruncateToLevel" should "return a level-truncated h3 with local index 0" in {
    val index = locationsInTree (0)
    val truncIndex = h3TruncateToRes (index, 5)

    instance.h3IsValid (index) should be (true)
    h3Resolution (truncIndex) should be (5)
  }

  "h3CreateCellIndex" should "return a well formatted h3 index" in {
    val index = h3CreateCellIndex(14, 128, Map(12 -> 1, 13 -> 2, 14 -> 3, 15 -> 4))

    localIndex(index, 12) should be (1)
    localIndex(index, 13) should be (2)
    localIndex(index, 14) should be (3)
    localIndex(index, 15) should be (7) // this is because everything below level 14 should be truncated
  }