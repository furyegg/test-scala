package observatory

import java.time.LocalDate

import scala.collection.parallel.ParSeq

object Main extends App {
  // week1
  val yearlyTemperatures: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(2015, "stations.csv", "2015.csv")
  val yearlyAverageRecords: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(yearlyTemperatures)
  
  val locations = for {
    lat <- 90 to -89 by -1
    lon <- -180 to 179
  } yield Location(lat, lon)
  
  // week2
  val parLocations = ParSeq(locations.toArray: _*)
  val predictedTemperatures: Iterable[(Location, Double)] =
    parLocations.map(loc=> (loc, Visualization.predictTemperature(yearlyAverageRecords, loc))).toList
  
  val colors: Iterable[(Double, Color)] = predictedTemperatures.map(t => (
      t._2,
      Visualization.interpolateColor(predefinedColors, t._2)
  ))
  
  // week3
  val zoomRange = 0
//  for {
//    zoom <- 0 to zoomRange
//    count = Math.pow(2, 2 * zoom)
//    x <- 0 until count
//    y <- 0 until count
//  }
  
  def predefinedColors(): Iterable[(Double, Color)] = {
    List(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    )
  }
}