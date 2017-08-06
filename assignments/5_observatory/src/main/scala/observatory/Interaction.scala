package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  
  val predefinedColors: Iterable[(Double, Color)] = List(
    (60, Color(255, 255, 255)),
    (32, Color(255, 0, 0)),
    (12, Color(255, 255, 0)),
    (0, Color(0, 255, 255)),
    (-15, Color(0, 0, 255)),
    (-27, Color(255, 0, 255)),
    (-50, Color(33, 0, 107)),
    (-60, Color(0, 0, 0))
  )
  
  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
      x.toDouble / (1 << zoom) * 360.0 - 180.0
    )
  }
  
  private def doubleToInt(n: Double): Int = BigDecimal(n).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt
  
  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val size = 256
    
    val coordinates = for {
      lon <- 0 until size
      lat <- 0 until size
    } yield (lat, lon)
  
    val pixels = coordinates.toParArray.map(p => {
      val loc = tileLocation(zoom + 8, p._1, p._2)
      val temperature = Visualization.predictTemperature(temperatures, loc)
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 127)
    }).toArray
  
    Image(size, size, pixels)
  }
  
  def generateImage(year: Int, zoom: Int, x: Int, y: Int,
    data: (Iterable[(Location, Double)], Iterable[(Double, Color)])): Unit = {
    
    def writeImage(image: Image, fileName: String): Unit = image.output(fileName)
    
    val image = tile(data._1, data._2, zoom, x, y)
    OutputHelper.createImage(year, zoom, x, y, image, writeImage)
  }
  
  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val zoomRange = 0
    for {
      (year, data) <- yearlyData
      zoom <- 0 to zoomRange
      tileCount = 2 * zoom
      x <- 0 to tileCount
      y <- 0 to tileCount
    } generateImage(year, zoom, x, y, data)
  }
  
}