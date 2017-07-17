package observatory

import java.time.LocalDate

import org.apache.commons.lang3.StringUtils

import scala.collection.parallel.ParSeq
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {
  
  private case class Station(stn: String, wban: String, latitude: Double, longitude: Double) {
    val id = (stn, wban)
  }
  
  private case class Temperature(stn: String, wban: String, month: Int, day: Int, fahrenheit: Double) {
    val celsius: Double = (fahrenheit - 32) / 1.8
    val invalid: Boolean = fahrenheit > 9999
    val stationId = (stn, wban)
  }
  
  def loadCsv(file: String): ParSeq[Array[String]] = {
    ParSeq(Source.fromInputStream(getClass.getResourceAsStream("/" + file)).getLines()
      .map(_.split(","))
      .toArray: _*)
  }
  
  private def loadStations(stationsFile: String): Iterable[Station] = {
    val lines = loadCsv(stationsFile)
    lines.map { l =>
      val stn = l(0)
      val wban = if (l.length < 2) "" else l(1)
      val lat = if (l.length < 3) "" else l(2)
      val lon = if (l.length < 4) "" else l(3)
      if (StringUtils.isBlank(lat) || StringUtils.isBlank(lon)) None
      else Some(Station(stn, wban, lat.toDouble, lon.toDouble))
    }.filter(_.isDefined).map(_.get).toList
  }
  
  private def loadTemperatures(temperaturesFile: String): Iterable[Temperature] = {
    val lines = loadCsv(temperaturesFile)
    lines.map { l =>
      val stn = l(0)
      val wban = l(1)
      val month = l(2)
      val day = l(3)
      val fah = l(4)
      Temperature(stn, wban, month.toInt, day.toInt, fah.toDouble)
    }.filter(!_.invalid).toList
  }
  
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = loadStations(stationsFile)
    if (stations.isEmpty) return Nil
    val stationMap = stations.map(s => (s.id -> s)).toMap
    
    val temperatures = loadTemperatures(temperaturesFile)
    temperatures.map(t => {
      val station = stationMap.get(t.stationId)
      if (station.isEmpty)
        None
      else {
        val date = LocalDate.of(year, t.month, t.day)
        val s = station.get
        val loc = Location(s.latitude, s.longitude)
        Some((date, loc, t.celsius))
      }
    }).filter(_.isDefined).map(_.get)
  }
  
  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    require(Set(records.map(_._1.getYear)).size == 1, "unable to calculate average more than one year")
    records.groupBy(_._2).mapValues(rs => {
      val temperatures = rs.map(_._3)
      val total = temperatures.foldLeft((0, 0.0))((acc, n) => (acc._1 + 1, acc._2 + n))
      total._2 / total._1
    }).toList
  }
  
}