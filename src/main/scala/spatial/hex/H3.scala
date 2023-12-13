package spatial.hex

import com.uber.h3core.util.LatLng
import com.uber.h3core.{H3Core, LengthUnit}

import scala.annotation.tailrec
import scala.util.Random


lazy val instance: H3Core = H3Core.newInstance

opaque type H3 = Long

object H3:
  def safe (impl: Long): H3 =
    assert (instance.isValidCell (impl))
    impl

  def apply (impl: Long): H3 =
    impl

  def unsafe (impl: Long): H3 =
    impl

  val unset: H3 = 0L
  lazy val minRes = 0
  lazy val maxRes = 15

  private lazy val allOnes: H3 = -1L
  private lazy val baseCellShift = 45
  private lazy val resShift = 52
  private lazy val cellModeShift = 59
  private lazy val cellModeMask: H3 = 1L << cellModeShift
  private lazy val locationMask: H3 = binaryOnes (52)
  private lazy val resMask: H3 = binaryOnes (4) << resShift
  private lazy val baseCellMask: H3 = binaryOnes (7) << baseCellShift
  private lazy val inBaseMask: H3 = binaryOnes (45)
  private lazy val zero: H3 = unset
  private lazy val zeroLocationOnly: H3 = allOnes & ~locationMask
  private lazy val zeroResOnly: H3 = allOnes & ~resMask

  transparent inline def fromLatLng (latLng: LatLng, res: Int): H3 =
    H3.safe (instance.latLngToCell (latLng.lat, latLng.lng, res))

  /**
   * binaryOnes - produces 2^n - 1 or n ones in binary
   * @param n \in {0, ..., 63} - number of ones to produce.
   * @return
   */
  transparent inline def binaryOnes (n: Int): H3 =
    (1L << n) - 1L

  /**
   * treeNodeMask - mask out the index coordinate at the requested resolution
   *
   * @param res - resolution between (between h3MinRes + 1 - h3MaxRes)
   * @return
   */
  transparent inline def treeNodeMask (res: Int): H3 =
    H3.unsafe (7L << (3 * (maxRes - res)))

  /**
   * H3 index mode is hardcoded to 1 presently, as we have no applications
   * other than containment.
   *
   * @param res      - cell resolution.  Takes values between h3MinRes-h3MaxRes.
   * @param baseCell - which of the 121 base cells is specified.
   * @param hex    - indexing to precisely locate a position within the H3 hierarchy.
   * @return
   */
  transparent inline def createCellIndex (res: Int, baseCell: Int, hex: H3): H3 =
    cellModeMask | (res.toLong << resShift) | (baseCell.toLong << baseCellShift) | hex

  /**
   * zeroBitCell - zero out parts of a H3 index
   *
   * @param res     - hex resolution
   * @param numBits - number of places to mask out.
   * @return
   */
  transparent inline def zeroBitCell (res: Int, numBits: Int): H3 =
    ~bitCellMask (res, numBits)

  /**
   * H3 index mode is hardcoded to 1 presently, as we have no applications
   * other than containment.
   *
   * @param res              - cell resolution.  Takes values between h3MinRes-h3MaxRes.
   * @param baseCell         - which of the 121 base cells is specified.
   * @param res2Local        - A map containing resolution mapping to local indexes, missing resolutions will be replaced with 0s
   * @return
   */
  transparent inline def createCellIndex (res: Int, baseCell: Int, res2Local: Map [Int, Int]): H3 =
    val index: H3 = res2Local.foldLeft (unset) { case (acc, (res, localIdx)) =>
      acc or globalIndexPart (localIdx, res)
    }
    createCellIndex (res, baseCell, index.truncateToRes (res))

  private transparent inline def generateValidBase (fromRes: Int, toRes: Int): H3 =
    ((maxRes - toRes) until (maxRes - fromRes))
      .foldLeft (unset) { (agg, rhs) =>
        agg | (Random.between (zero, 7L) << (3 * rhs))
      } | bitCellMask (toRes, 3)

  /**
   * FIXME - some base indices fail.  Probably due to pent-hex issues.  Trouble with base hex 10
   * FIXME - the loop is a HACK to fix the above.  Probably won't fix.
   * for instance
   * generate random hex with resolution dependencies
   * FIXME - there are some "special" properties that you may not want:
   *  - every resolution below fromRes that is not randomised left out defaults to h3MinRes0 (centre hex)
   *  - every resolution above toRes defaults to not used
   *    Will fix this when usage needs are clearer.  In meantime, there is also h3RandomInHex
   *    to consider
   *
   * @param baseCell - this is one of 121 base resolution h3 hexes that we index from.
   * @param fromRes  - hexes at this resolution and above are randomised
   * @param toRes    - hexes at this resolution and below are randomised
   * @return
   */
  transparent inline def random (baseCell: Int, fromRes: Int, toRes: Int): H3 =
    var index = unset
    while
      index = unsafeRandom (baseCell, fromRes, toRes)
      !instance.isValidCell (index)
    do
      index
    index

  transparent inline def unsafeRandom (baseCell: Int, fromRes: Int, toRes: Int): H3 =
    cellModeMask | (toRes.toLong << resShift) | (baseCell.toLong << baseCellShift) | generateValidBase (fromRes, toRes)

  /**
   * bitCellMask - mask out higher order parts of a H3 index.
   *
   * @param res     - hex resolution
   * @param numBits - number of lower levels to preserve.
   * @return
   */
  transparent inline def bitCellMask (res: Int, numBits: Int): H3 =
    binaryOnes ((maxRes - res) * numBits)

  /**
   * get the global part of H3 index at res
   *
   * @param localIndex - local index value
   * @param res        - the resolution required (between h3MinRes + 1 - h3MaxRes)
   * @return - the global part of H3 index
   */
  transparent inline def globalIndexPart (localIndex: Int, res: Int): H3 =
    H3.unsafe ((treeNodeMask (maxRes) & localIndex) << (3 * (maxRes - res)))

  /**
   * driveTimeInS - drive time in seconds between from and to
   *
   * @param speedInMpS - speed in metres / sec
   * @param from       - valid H3 location
   * @param to         - valid H3 location
   * @return - time in seconds
   */
  def driveTimeInS (speedInMpS: Double, from: H3, to: H3): Long =
    from.distanceInM (to).toLong / speedInMpS.toLong

  /**
   * marginalTime - calculates the difference in travel time between
   *                1. begin -> end and
   *                   2. begin -> insert + insert -> end?
   *
   * @param begin      - valid H3 location
   * @param end        - valid H3 location
   * @param insert     - valid H3 location
   * @param speedInMpS - speed in metres per second
   * @return - marginal time in seconds
   */
  def marginalTimeInS (begin: H3, end: H3, insert: H3, speedInMpS: Double): Long =
    driveTimeInS (speedInMpS, begin, insert) + driveTimeInS (speedInMpS, insert, end) - driveTimeInS (speedInMpS, begin, end)


  extension (hex: H3)
    transparent inline def isValid: Boolean =
      instance.isValidCell (hex)

    transparent inline def isInvalid: Boolean =
      !isValid

    transparent inline def toLatLng: LatLng =
      instance.cellToLatLng (hex)

    transparent inline def binaryString: String =
      hex.toBinaryString

    transparent inline def or (rhs: H3): H3 =
      hex | rhs

    /**
     * distance - checks the linear distance between lhs and rhs in terms of unit
     *
     * @param rhs  - valid H3 location
     * @param unit - unit of linear measure
     * @return number of units between lhs and rhs
     */
    transparent inline def distance (rhs: H3, unit: LengthUnit): Double =
      instance.greatCircleDistance (hex.toLatLng, rhs.toLatLng, unit)

    /**
     * distanceInKm - linear distance in km between lhs and rhs
     *
     * @param rhs - valid H3 location
     * @return - kilometres between lhs and rhs
     */
    transparent inline def distanceInKm (rhs: H3): Double =
      hex.distance (rhs, LengthUnit.km)

    /**
     * distanceInM - linear distance in m between lhs and rhs
     *
     * @param rhs - valid H3 location
     * @return - metres between lhs and rhs§
     */
    transparent inline def distanceInM (rhs: H3): Double =
      hex.distance (rhs, LengthUnit.m)

    def inBaseTruncate: H3 =
      hex & inBaseMask

    /**
     * resolution - return the resolution part of an h3 index
     */
    transparent inline def resolution: Int =
      ((hex & resMask) >> resShift).toInt

    def explain: String =
      val indexRes = hex.resolution

      s"Resolution = $resolution [${
        (minRes to indexRes).foldLeft ("") ((agg, res) => s"$agg ${hex.local (res)}")
      }]"

    /**
     * local - get the local coordinate of index at res
     *
     * @param res - the resolution required (between h3MinRes + 1 - h3MaxRes)
     * @return - the local index
     */
    transparent inline def local (res: Int): Int =
      ((treeNodeMask (res) & hex) >> (3 * (maxRes - res))).toInt

    /**
     * unsafeRandomInHex - generates a random H3 index that is contained in a hex of resolution <res> created by trunctating
     * <baseHex> index.  Unsafe because H3 will not always be valid if generated in a base pentagon.
     *
     * @param res - hex resolution
     * @return
     */
    transparent inline def unsafeRandomInHex (res: Int): H3 =
      hex.zeroToRes (res) | generateValidBase (res, maxRes)

    /**
     * generate random positions that are in the same hex as the centroid of the
     * resolution base hexagon containing base.
     *
     * @return - a random hex location from within hex base
     */
    def randomInHex (res: Int): H3 =
      var index = unset
      while
        index = hex unsafeRandomInHex res
        index.isInvalid
      do
        index
      index

    /**
     * centre - get the centre point of the cell determined by an H3 index
     *
     * @param res - cell resolution
     * @return - minimum resolution index at centre of cell.  Considered a point.
     */
    inline def centre (res: Int): H3 =
      instance.cellToCenterChild (hex, res)

    /**
     * truncates an index to a given resolution.
     *
     * @param res - resolution, assumed to be in {h3MinRes, ..., h3MaxRes}.  This is not protected in any way.
     * @return
     */
    transparent inline def truncateToRes (res: Int): H3 =
      (hex & zeroResOnly) | bitCellMask (res, 3) | (res.toLong << resShift)

    /**
     * zeroes all indices higher than res in the index.
     * Note - this specifies the centre point for the cell represented by truncateToRes (index, res)
     *
     * @param res - resolution of the cell for which we compute a centre.
     * @return
     */
    transparent inline def zeroToRes (res: Int): H3 =
      (hex >> 3 * (maxRes - res)) << 3 * (maxRes - res)

    /**
     * baseHex - returns the top resolution base index that contains index
     *
     * @return - base hex containing h3.
     */
    transparent inline def baseHex: Short =
      ((hex & baseCellMask) >> baseCellShift).toShort

    /**
     * contains - checks is location lies in hex
     *
     * @param location - any valid h3 hex
     * @return - true if location in hex, false otherwise
     */
    transparent inline def contains (location: H3): Boolean =
      (hex & locationMask) == ((location & locationMask) | bitCellMask (hex.resolution, 3))
        && hex.baseHex == location.baseHex
