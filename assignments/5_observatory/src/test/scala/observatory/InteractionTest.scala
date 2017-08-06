package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  
  test("pixel to location") {
    val zoom = 8
    println(Interaction.tileLocation(zoom, 0, 0))
    println(Interaction.tileLocation(zoom, 0, 256))
    println(Interaction.tileLocation(zoom, 256, 0))
    println(Interaction.tileLocation(zoom, 256, 256))
    println(Interaction.tileLocation(zoom, 128, 128))
    println(Interaction.tileLocation(zoom, 64, 64))
  }
  
  test("generate image for 2021") {
    val year = 2021
    val yearlyTemperatures: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(year, "stations3.csv", "2021.csv")
    val yearlyAverageRecords: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(yearlyTemperatures)
    val yearlyData = List((year, (yearlyAverageRecords, Interaction.predefinedColors)))
    Interaction.generateTiles(yearlyData, Interaction.generateImage)
  }
}