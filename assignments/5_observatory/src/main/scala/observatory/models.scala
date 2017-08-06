package observatory

import scala.math._

case class Location(lat: Double, lon: Double) {
  private val R = 6371e3
  private val p = 2
  
  def distanceTo(loc: Location): Double = {
    val dLat = (loc.lat - lat).toRadians
    val dLon = (loc.lon - lon).toRadians
    
    val a = sin(dLat / 2) * sin(dLat / 2) + cos(lat.toRadians) * cos(loc.lat.toRadians) * sin(dLon / 2) * sin(dLon / 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    
    R * c
  }
}

case class Color(red: Int, green: Int, blue: Int)