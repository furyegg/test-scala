package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  
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
      lon <- y * size until (y + 1) * size
      lat <- x * size until (x + 1) * size
    } yield (lat, lon)
  
    val pixels = coordinates.toParArray.map({ case (x, y) =>
      val loc = tileLocation(zoom + 8, x, y)
      val temperature = Visualization.predictTemperature(temperatures, loc)
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 127)
    }).toArray
  
    Image(size, size, pixels)
  }
  
  def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: (Iterable[(Location, Double)], Iterable[(Double, Color)])): Unit = {
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
    val zoomRange = 1
    for {
      (year, data) <- yearlyData
      zoom <- 0 to zoomRange
      size = pow(2, zoom).toInt
      x <- 0 until size
      y <- 0 until size
    } generateImage(year, zoom, x, y, data)
  }
  
}