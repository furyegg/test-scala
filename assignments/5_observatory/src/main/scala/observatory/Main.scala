package observatory

import java.time.LocalDate

object Main extends App {
  private val year = 2015
  
  // week1
  val yearlyTemperatures: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(year, "stations.csv", "2015.csv")
  val yearlyAverageRecords: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(yearlyTemperatures)
  
  // week3
  val yearlyData = List((year, (yearlyAverageRecords, Visualization.predefinedColors)))
  Interaction.generateTiles(yearlyData, Interaction.generateImage)
  
}