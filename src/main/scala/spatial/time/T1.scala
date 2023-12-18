package spatial.time


opaque type T1 = Long

enum TimeScheme:
  case SIXTEENTH_SECOND, EIGHTH_SECOND,
    QUARTER_SECOND, HALF_SECOND, SECOND,
    QUARTER_MINUTE, MINUTE, QUARTER_HOUR,
    HALF_HOUR, HOUR, DAY, WEEK, MONTH,
    QUARTER, YEAR

/**
 * We want to preserve structure of seconds -> minutes -> hours -> days, weeks, months, years in our hierarchy.
 *
 * 15 * 4 - 4 quarters of a minute or 4 quarters of an hour.
 *  64 - 2^6, 128 - 2^7, 256 - 2^8, 512 - 2^9, 1024 - 2^10, 2048 - 2^11, 4096 - 2^12.
 *  24 - 3 * 8, 168 - 7 * 24
 *  So [26-27] [24-25] [18-23] [13-17] [7-12] [0-6]
 *        ^       ^       ^       ^      ^      ^
 *      qt-yr   mt-qt    d-mt   hr-d   mn-hr   s-mn
 */

object T1:
  def validate (idx: T1): Boolean =
    ???
