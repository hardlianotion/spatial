package spatial

import com.uber.h3core.util.LatLng

import scala.language.postfixOps

import spatial.hex.H3


object DeliveryMocks:
  // h3 addresses
  val testTreeLevel = 5
  // NOTE -  8f39601ae658180 somewhere in Toulouse, I think
  val testHubAddress: H3 = H3.unsafe (java.lang.Long.valueOf ("8f39601ae658180", 16))
  val locationsInTree: Seq [H3] = (1 to 10).map (_ => testHubAddress.randomInHex (testTreeLevel))

  // NOTE - these are generated to be outside any sensible tree containing testHubAddress
  val josZoo: H3 = H3.unsafeFromLatLng (LatLng (9.920420123563392, 8.887754970466519), 15)
  val locationsInNigeria: Seq [H3] = (1 to 10).map (_ => josZoo.randomInHex (13))
