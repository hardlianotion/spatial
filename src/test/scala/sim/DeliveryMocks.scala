package sim

import com.uber.h3core.util.LatLng
import java.time.{Duration, Instant}

import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Set, Map as MutableMap}
import scala.language.postfixOps

import zio.prelude.NonEmptyList as NEL
import sim.hex.{H3, H3Tree, h3Random, h3RandomInHex}


object DeliveryMocks:
  // h3 addresses
  val testTreeLevel = 5
  // NOTE -  8f39601ae658180 somewhere in Toulouse, I think
  val testHubAddress = java.lang.Long.valueOf ("8f39601ae658180", 16)
  val locationsInTree = (1 to 10).map (_ => h3RandomInHex (testHubAddress, testTreeLevel))

  // NOTE - these are generated to be outside any sensible tree containing testHubAddress
  val josZoo = hex.instance.latLngToCell (9.920420123563392, 8.887754970466519, 15)
  val locationsInNigeria = (1 to 10).map (_ => h3RandomInHex (josZoo, 13))

object RegionMocks:
  import sim.DeliveryMocks.*
