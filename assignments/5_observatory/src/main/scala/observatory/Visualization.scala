package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math.{atan2, cos, sin, sqrt}
import scala.util.Sorting

/**
  * 2nd milestone: basic visualization
  * https://en.wikipedia.org/wiki/Great-circle_distance
  * https://en.wikipedia.org/wiki/Inverse_distance_weighting
  */
object Visualization {
  
  val temperaturePredefinedColors: Iterable[(Double, Color)] = List(
    (60, Color(255, 255, 255)),
    (32, Color(255, 0, 0)),
    (12, Color(255, 255, 0)),
    (0, Color(0, 255, 255)),
    (-15, Color(0, 0, 255)),
    (-27, Color(255, 0, 255)),
    (-50, Color(33, 0, 107)),
    (-60, Color(0, 0, 0))
  )
  
  def distance(knowLoc: Location, loc: Location): Double = {
    val dLat = (loc.lat - knowLoc.lat).toRadians
    val dLon = (loc.lon - knowLoc.lon).toRadians
    
    val a = sin(dLat / 2) * sin(dLat / 2) + cos(knowLoc.lat.toRadians) * cos(loc.lat.toRadians) * sin(dLon / 2) * sin(dLon / 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
  
    6371000 * c
  }
  
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
//    var totalTemp = 0.0
//    var totalDist = 0.0
//    val itr = temperatures.iterator
//    while (itr.hasNext) {
//      val (knowLoc, knowTemp) = itr.next()
//      val dist = knowLoc.distanceTo(location)
//      if (dist < 1000) {
//        return knowTemp
//      } else {
//        val wix = 1 / pow(dist, 3)
//        totalTemp += wix * knowTemp
//        totalDist += wix
//      }
//    }
//    totalTemp / totalDist
    
    val sum = temperatures.aggregate((0.0, 0.0))(
      {
        case ((tempSum, distSum), (loc, temp)) => {
          val dist = distance(loc, location)
          if (dist < 1000) {
            (tempSum + temp, distSum)
          } else {
            val wix = 1 / pow(dist, 3)
            (tempSum + wix * temp, distSum + wix)
          }
        }
      },
      (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2)
    )
    sum._1 / sum._2
  }
  
  private object PointOrdering extends Ordering[(Double, Color)] {
    override def compare(x: (Double, Color), y: (Double, Color)): Int = x._1.compareTo(y._1)
  }
  
  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    require(!value.isNaN, "invalid temperature: " + value)
    
    // check range
    val sortedPoints = points.toArray
    Sorting.quickSort(sortedPoints)(PointOrdering)
    
    val firstPoint = sortedPoints(0)
    val lastPoint = sortedPoints(sortedPoints.length - 1)
    if (value < firstPoint._1) return firstPoint._2
    if (value > lastPoint._1) return lastPoint._2
    
    // check match
    val foundColor = points.find(_._1 == value)
    if (foundColor.isDefined)
      foundColor.get._2
    else {
      val newPoint = (value, Color(0, 0, 0))
      val newSortedPoints = points ++: Array(newPoint)
      Sorting.quickSort(newSortedPoints)(PointOrdering)
      
      val newPointIndex = newSortedPoints.indexWhere(_._1 == newPoint._1)
      val previousPoint = newSortedPoints(newPointIndex - 1)
      val nextPoint = newSortedPoints(newPointIndex + 1)
      interpolateColor(previousPoint, newPoint, nextPoint)
    }
  }
  
  private def interpolateColor(
      previousPoint: (Double, Color),
      newPoint: (Double, Color),
      nextPoint: (Double, Color)): Color = {
    
    val radio = calcColorRadio(previousPoint._1, newPoint._1, nextPoint._1)
    val r = calcRGB(previousPoint._2.red, nextPoint._2.red, radio)
    val g = calcRGB(previousPoint._2.green, nextPoint._2.green, radio)
    val b = calcRGB(previousPoint._2.blue, nextPoint._2.blue, radio)
    Color(r, g, b)
  }
  
  private def calcColorRadio(temp1: Double, temp2: Double, temp3: Double): Double =
    (temp3 - temp1) / (temp2 - temp1)
  
  private def calcRGB(c1: Int, c2: Int, radio: Double): Int = {
    val diff = Math.abs(c2 - c1)
    if (diff == 0) return c1
    
    val sign = if (c1 < c2) 1 else -1
    val change = diff / radio * sign
    
    val res = BigDecimal(c1 + change)
    res.setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt
  }
  
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val coordinates = for {
      lon <- -180 to 179
      lat <- 90 to -89 by -1
    } yield (lat, lon)
  
    val pixels = coordinates.toParArray.map { case (lat, lon) => {
      val temp = predictTemperature(temperatures, Location(lat, lon))
      val color = interpolateColor(temperaturePredefinedColors, temp)
      Pixel(color.red, color.green, color.blue, 127)
    }}.toArray
    
    Image(360, 180, pixels)
  }
  
}