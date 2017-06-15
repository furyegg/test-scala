package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  test("extraction 1") {
    val records = Extraction.locateTemperatures(2001, "/stations.csv", "/2001.csv")
    println(records)
    val avg = Extraction.locationYearlyAverageRecords(records)
    println(avg)
  }
  
  test("extraction 2") {
    val records = Extraction.locateTemperatures(2001, "/stations1.csv", "/2001.csv")
    println(records)
    val avg = Extraction.locationYearlyAverageRecords(records)
    println(avg)
  }
  
  test("extraction empty stations") {
    val records = Extraction.locateTemperatures(2001, "/empty-stations.csv", "/2001.csv")
    println(records)
    val avg = Extraction.locationYearlyAverageRecords(records)
    println(avg)
  }
}