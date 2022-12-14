package sim.math

import scala.language.implicitConversions


object Aggregable:
  implicit object IntIsAggregable extends Aggregable [Int, Int]:
    def add (agg:  Int, inc:  Int): Int =
      agg + inc
    def count (agg: Int) = 1L
    def empty: Int = 0
    def isEmpty (agg: Int): Boolean =
      agg == 0

  implicit object LongIsAggregable extends Aggregable [Long, Long]:
    def add (agg:  Long, inc:  Long): Long =
      agg + inc
    def count (agg: Long) = 1L
    def empty: Long = 0L
    def isEmpty (agg: Long): Boolean =
      agg == 0L

trait Aggregable [A, T]:
  def add (agg: A, inc: T): A
  def count (agg: A): Long
  def empty: A
  def isEmpty (agg: A): Boolean

  class AggregationOps (agg: A):
    def + (rhs: T) = add (agg, rhs)
    def count = Aggregable.this.count (agg)

  implicit def mkAggregableOps (a: A): AggregationOps =
     new AggregationOps (a)

