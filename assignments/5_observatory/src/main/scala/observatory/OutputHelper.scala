package observatory

import java.io.File

import com.sksamuel.scrimage.Image
import org.apache.commons.io.FileUtils

object OutputHelper {
  private val rootPath = "target/temperatures"
  
  def createYearPath(year: Int): String = {
    val yearPath = s"${rootPath}/${year}/"
    val yearPathFile = new File(yearPath)
    if (!yearPathFile.exists) FileUtils.forceMkdir(yearPathFile)
    yearPath
  }
  
  def createZoomPath(year: Int, zoom: Int): String = {
    val zoomPath = createYearPath(year) + "/" + zoom + "/"
    val zoomPathFile = new File(zoomPath)
    if (!zoomPathFile.exists) FileUtils.forceMkdir(zoomPathFile)
    zoomPath
  }
  
  def createImage(year: Int, zoom: Int, x: Int, y: Int, image: Image, writeImage: (Image, String) => Unit): Unit = {
    val imageFile = s"${createZoomPath(year, zoom)}/${x}-${y}.png"
    writeImage(image, imageFile)
  }
  
}