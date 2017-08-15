package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  
//  test("generate image for 2021") {
//    val year = 2021
//    val yearlyTemperatures: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(year, "stations3.csv", "2021.csv")
//    val yearlyAverageRecords: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(yearlyTemperatures)
//    val yearlyData = List((year, (yearlyAverageRecords, Visualization.predefinedColors)))
//    Interaction.generateTiles(yearlyData, Interaction.generateImage)
//  }
  
}
