package sim.hex

import com.uber.h3core.{H3Core, LengthUnit}

import scala.annotation.tailrec
import scala.util.Random


lazy val instance: H3Core = H3Core.newInstance

type H3 = Long
inline val h3Null = 0L
inline val h3MinRes = 0
inline val h3MaxRes = 15

private inline val h3AllOnes = -1L
private inline val h3BaseCellShift = 45
private inline val h3ResShift = 52
private inline val h3CellModeShift = 59
private inline val h3CellModeMask = 1L << h3CellModeShift
private inline val h3LocationMask = h3BinaryOnes (52)
private inline val h3ResMask  = h3BinaryOnes (4) << h3ResShift
private inline val h3BaseCellMask = h3BinaryOnes (7) << h3BaseCellShift
private inline val h3InBaseMask = h3BinaryOnes (45)
private inline val h3Zero = 0
private inline val h3ZeroLocationOnly = h3AllOnes & ~h3LocationMask
private inline val h3ZeroResOnly = h3AllOnes & ~h3ResMask


def h3ExplainAddress (index: H3): String =
  val root = s"Resolution = ${h3Resolution (index)} ["
  val resolution = h3Resolution (index)
  val result =
    (h3MinRes to resolution)
      .foldLeft (root) ((agg, res) => s"$agg ${localIndex (index, res)}")

  s"$result]"

def h3InBaseTruncate (index: H3): Long =
  index & h3InBaseMask

/**
 * H3 index mode is hardcoded to 1 presently, as we have no applications
 * other than containment.
 *
 * @param res - cell resolution.  Takes values between 0-h3MaxRes.
 * @param baseCell - which of the 121 base cells is specified.
 * @param index - indexing to precisely locate a position within the H3 hierarchy.
 * @return
 */
transparent inline def h3CreateCellIndex (res: Int, baseCell: Int, index: Long): H3 =
  h3CellModeMask | (res.toLong << h3ResShift) | (baseCell.toLong << h3BaseCellShift) | index

/**
 * H3 index mode is hardcoded to 1 presently, as we have no applications
 * other than containment.
 *
 * @param res - cell resolution.  Takes values between 0-h3MaxRes.
 * @param baseCell - which of the 121 base cells is specified.
 * @param res2LocalIndexes - A map containing resolution mapping to local indexes, missing resolutions will be replaced with 0s
 * @return
 */
transparent inline def h3CreateCellIndex (res: Int, baseCell: Int, res2LocalIndexes: Map[Int, Int]): H3 =
  val index = res2LocalIndexes.foldLeft (h3Null) { case (acc, (res, localIdx)) =>
    acc | globalIndexPart (localIdx, res)
  }
  h3CreateCellIndex (res, baseCell, h3TruncateToRes (index, res))

private transparent inline def generateValidH3Base (fromRes: Int, toRes: Int): H3 =
  ((h3MaxRes - toRes) until (h3MaxRes - fromRes))
    .foldLeft (h3Null) { (agg, rhs) =>
      agg  | (Random.between (h3Zero, 7L) << (3 * rhs))
    } | h3BitCellMask (toRes, 3)

/**
 * FIXME - some base indices fail.  Probably due to pent-hex issues.  Trouble with base hex 10
 * FIXME - the loop is a HACK to fix the above.  Probably won't fix.
 * for instance
 * generate random hex with resolution dependencies
 * FIXME - there are some "special" properties that you may not want:
 *  - every resolution below fromRes that is not randomised left out defaults to h3MinRes0 (centre hex)
 *  - every resolution above toRes defaults to not used
 *  Will fix this when usage needs are clearer.  In meantime, there is also h3RandomInHex
 *  to consider
 *
 * @param baseCell - this is one of 121 base resolution h3 hexes that we index from.
 * @param fromRes - hexes at this resolution and above are randomised
 * @param toRes - hexes at this resolution and below are randomised
 * @return
 */
transparent inline def h3Random (baseCell: Int, fromRes: Int, toRes: Int): H3 =
  var index = h3Null
  while
    index = unsafeH3Random (baseCell, fromRes, toRes)
    !instance.isValidCell (index)
  do
    index
  index

transparent inline def unsafeH3Random (baseCell: Int, fromRes: Int, toRes: Int): H3 =
    h3CellModeMask | (toRes.toLong << h3ResShift)  | (baseCell.toLong << h3BaseCellShift) | generateValidH3Base (fromRes, toRes)

/**
 * truncates an index to a given resolution.
 * @param index - input index with h3 format
 * @param res - resolution, assumed to be in {h3MinRes, ..., h3MaxRes}.  This is not protected in any way.
 * @return
 */
transparent inline def h3TruncateToRes (index: H3, res: Int): H3 =
  (index & h3ZeroResOnly) | h3BitCellMask (res, 3) | (res.toLong << h3ResShift)

/**
 * zeroes all indices higher than res in the index.
 * Note - this specifies the centre point for the cell represented by h3TruncateToRes (index, res)
 * @param index - index for the cell input
 * @param res - resolution of the cell for which we compute a centre.
 * @return
 */
transparent inline def h3ZeroToRes (index: H3, res: Int): H3 =
  (index >> 3 * (h3MaxRes - res)) << 3 * (h3MaxRes - res)
/**
 * generate random positions that are in the same hex as the centroid of the
 * resolution base hexagon containing base.
 * @param base - valid H3 hex
 * @return - a random hex location from within hex base
 */
def h3RandomInHex (base: H3, res: Int): H3 =
  var index = h3Null
  while
    index = unsafeH3RandomInHex (base, res)
    !instance.isValidCell (index)
  do
    index
  index
/**
 * unsafeH3RandomInHex - generates a random H3 index that is contained in a hex of resolution <res> created by trunctating
 *                        <baseHex> index.  Unsafe because H3 will not always be valid if generated in a base pentagon.
 * @param baseHex
 * @param res
 * @return
 */
transparent inline def unsafeH3RandomInHex (baseHex: H3, res: Int): H3 =
  h3ZeroToRes (baseHex, res)  | generateValidH3Base (res, h3MaxRes)

/**
 * h3BinaryOnes - produces 2^n - 1 or n ones in binary
 * @param n \in {0, ..., 63} - number of ones to produce.
 * @return
 */
transparent inline def h3BinaryOnes (n: Int): Long =
  (1L << n) - 1L

/**
 * h3BitCellMask - mask out parts of a H3 index.
 * @param res - hex resolution
 * @param numBits
 * @return
 */
transparent inline def h3BitCellMask (res: Int, numBits: Int): Long =
  h3BinaryOnes ((h3MaxRes - res) * numBits)

/**
 * h3ZeroBitCell - zero out parts of a H3 index
 * @param res
 * @param numBits
 * @return
 */
transparent inline def h3ZeroBitCell (res: Int, numBits: Int): Long =
  ~h3BitCellMask (res, numBits)

/**
 * h3Resolution - return the reslution part of an h3 index.
 * @param index
 * @return
 */
transparent inline def h3Resolution (index: H3): Int =
  ((index & h3ResMask) >> h3ResShift).toInt

/**
 * h3TreeNodeMask - mask out the index coordinate at the requested resolution
 * @param res - resolution between (between 1 - h3MaxRes)
 * @return
 */
transparent inline def h3TreeNodeMask (res: Int): Long =
  7L << (3 * (h3MaxRes - res))


/**
 * h3Centre - get the centre point of the cell determined by an H3 index
 * @param cell - the h3 cell
 * @param res - cell resolution
 * @return - minimum resolution index at centre of cell.  Considered a point.
 */
inline def h3Centre (cell: H3, res: Int): H3 =
  instance.cellToCenterChild (cell, res)

/**
 * localIndex - get the local coordinate of index at res
 * @param index - the h3 index
 * @param res - the resolution required (between 1 - h3MaxRes)
 * @return - the local index
 */
transparent inline def localIndex (index: H3, res: Int): Int =
  ((h3TreeNodeMask (res) & index) >> (3 * (h3MaxRes - res))).toInt

/**
 * get the global part of H3 index at res
 * @param localIndex - local index value
 * @param res - the resolution required (between 1 - h3MaxRes)
 * @return - the global part of H3 index
 */
transparent inline def globalIndexPart (localIndex: Int, res: Int): Long =
  (h3TreeNodeMask (h3MaxRes) & localIndex) << (3 * (h3MaxRes - res)).toLong

/**
 * baseHex - returns the top resolution base index that contains index
 * @param index - h3 index
 * @return - base hex containing h3.
 */
transparent inline def baseHex (index: H3): Short =
  ((index & h3BaseCellMask) >> h3BaseCellShift).toShort
/**
 * contains - checks is location lies in hex
 * @param hex - any valid h3 hex
 * @param location - any valid h3 hex
 * @return - true if location in hex, false otherwise
 */
transparent inline def contains (hex: H3, location: H3): Boolean =
  (hex & h3LocationMask) == ((location & h3LocationMask) | h3BitCellMask (h3Resolution (hex), 3))
  && baseHex (hex) == baseHex (location)

/**
 * distance - checks the linear distance between lhs and rhs in terms of unit
 * @param lhs - valid H3 location
 * @param rhs - valid H3 location
 * @param unit - unit of linear measure
 * @return number of units between lhs and rhs
 */
transparent inline def distance (lhs: H3, rhs: H3, unit: LengthUnit): Double =
  instance.greatCircleDistance (instance.cellToLatLng (lhs), instance.cellToLatLng (rhs), unit)

/**
 * distanceInKm - linear distance in km between lhs and rhs
 * @param lhs - valid H3 location
 * @param rhs - valid H3 location
 * @return - kilometres between lhs and rhs
 */
def distanceInKm (lhs: H3, rhs: H3): Double =
  distance (lhs, rhs, LengthUnit.km)

/**
 * distanceInM - linear distance in m between lhs and rhs
 * @param lhs - valid H3 location
 * @param rhs - valid H3 location
 * @return - metres between lhs and rhs
 */
def distanceInM (lhs: H3, rhs: H3): Double =
  distance (lhs, rhs, LengthUnit.m)

/**
 * driveTimeInS - drive time in seconds between from and to
 * @param speedInMpS - speed in metres / sec
 * @param from - valid H3 location
 * @param to - valid H3 location
 * @return - time in seconds
 */
def driveTimeInS (speedInMpS: Double, from: H3, to: H3): Long =
  distanceInM (from, to).toLong / speedInMpS.toLong


/**
 * marginalTime - calculates the difference in travel time between
 *                1. begin -> end and
 *                2. begin -> insert + insert -> end?
 * @param begin - valid H3 location
 * @param end - valid H3 location
 * @param insert - valid H3 location
 * @param speedInMpS - speed in metres per second
 * @return - marginal time in seconds
 */
def marginalTimeInS (begin: H3, end: H3, insert: H3, speedInMpS: Double): Long =
  driveTimeInS (speedInMpS, begin, insert) + driveTimeInS (speedInMpS, insert, end) - driveTimeInS (speedInMpS, begin, end)
