package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

import scala.util.Sorting

/**
  * 2nd milestone: basic visualization
  * https://en.wikipedia.org/wiki/Great-circle_distance
  * https://en.wikipedia.org/wiki/Inverse_distance_weighting
  */
object Visualization {
  
  private def distance(knowLoc: Location, location: Location): Double = {
    acos(
      sin(location.lon) * sin(knowLoc.lon) +
      cos(location.lat) * cos(knowLoc.lat) * cos(abs(location.lat - knowLoc.lat))
    ) * 6371000
  }
  
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    var totalTemp = 0.0
    var totalDist = 0.0
    for ((knowLoc, knowTemp) <- temperatures) {
      val dist = distance(knowLoc, location)
      if (dist < 1000)
        return knowTemp
      else {
        val wix = 1 / pow(dist, 3)
        totalTemp += wix * knowTemp
        totalDist += wix
      }
    }
    totalTemp / totalDist
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
  
  private def interpolateColor(previousPoint: (Double, Color), newPoint: (Double, Color), nextPoint: (Double, Color)): Color = {
    val colorRadio = calcColorRadio(previousPoint._1, newPoint._1, nextPoint._1)
    val r = calcRGB(previousPoint._2.red, nextPoint._2.red, colorRadio)
    val g = calcRGB(previousPoint._2.green, nextPoint._2.green, colorRadio)
    val b = calcRGB(previousPoint._2.blue, nextPoint._2.blue, colorRadio)
    Color(r, g, b)
  }
  
  private def calcColorRadio(temp1: Double, temp2: Double, temp3: Double): Double =
    (temp3 - temp1) / (temp2 - temp1)
  
  private def calcRGB(c1: Int, c2: Int, colorRadio: Double): Int = {
    val res = BigDecimal((c2.toDouble + c1.toDouble * colorRadio) / (colorRadio + 1))
    res.setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt
  }
  
  private object LocationLonOrdering extends Ordering[(Location, Double)] {
    override def compare(x: (Location, Double), y: (Location, Double)): Int = x._1.lon.compareTo(y._1.lon)
  }
  
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val pixels: Array[Pixel] = new Array(360 * 180)
    val colorMap: Map[Double, Color] = colors.groupBy(_._1).mapValues(_.head._2)
    var i = 0
    
    for (lat <- 90 to -89 by -1) {
      val points = temperatures.filter(p => p._1 == lat).toArray
      Sorting.quickSort(points)(LocationLonOrdering)
      require(points.length == 360)
      
      for (lon <- 0 until 360) {
        val temp = points(lon)._2
        val color = colorMap.get(temp)
        require(color.isDefined, "Unable to find color by temperature: " + temp)
        val c = color.get
        pixels(i) = Pixel(c.red, c.green, c.blue, 1)
        i += 1
      }
    }
    Image(360, 180, pixels)
  }
  
}