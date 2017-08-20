package observatory

import java.io.File

import com.sksamuel.scrimage.Image
import org.apache.commons.io.FileUtils

object OutputHelper {
  private val temperatureRootPath = "target/temperatures"
  private val deviationRootPath = "target/deviations"
  
  def createYearPath(rootPath:String, year: Int): String = {
    val yearPath = s"${rootPath}/${year}/"
    val yearPathFile = new File(yearPath)
    if (!yearPathFile.exists) FileUtils.forceMkdir(yearPathFile)
    yearPath
  }
  
  def createZoomPath(rootPath:String, year: Int, zoom: Int): String = {
    val zoomPath = createYearPath(rootPath, year) + "/" + zoom + "/"
    val zoomPathFile = new File(zoomPath)
    if (!zoomPathFile.exists) FileUtils.forceMkdir(zoomPathFile)
    zoomPath
  }
  
  def createImage(rootPath:String, year: Int, zoom: Int, x: Int, y: Int, image: Image, writeImage: (Image, String) => Unit): Unit = {
    val imageFile = s"${createZoomPath(rootPath, year, zoom)}/${x}-${y}.png"
    writeImage(image, imageFile)
  }
  
  def createTemperatureImage(year: Int, zoom: Int, x: Int, y: Int, image: Image, writeImage: (Image, String) => Unit): Unit = {
    createImage(temperatureRootPath, year, zoom, x, y, image, writeImage)
  }
  
  def createDeviationImage(year: Int, zoom: Int, x: Int, y: Int, image: Image, writeImage: (Image, String) => Unit): Unit = {
    createImage(deviationRootPath, year, zoom, x, y, image, writeImage)
  }
  
}