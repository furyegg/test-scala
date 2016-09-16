
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =
    if (radius == 0) src(x, y)
    else {
      val maxCol = src.width - 1
      val maxRow = src.height - 1
      val leftTop = (clamp(x - radius, 0, maxCol), clamp(y - radius, 0, maxRow))
      val rightBottom = (clamp(x + radius, 0, maxCol), clamp(y + radius, 0, maxRow))
      
      var allRed = List[Int]()
      var allGreen = List[Int]()
      var allBlue = List[Int]()
      var allAlpha = List[Int]()
      
      for {
        col <- leftTop._1 to rightBottom._1
        row <- leftTop._2 to rightBottom._2
        pixel = src(col, row)
      } {
        allRed :+= red(pixel)
        allGreen :+= green(pixel)
        allBlue :+= blue(pixel)
        allAlpha :+= alpha(pixel)
      }
      
      rgba(average(allRed), average(allGreen), average(allBlue), average(allAlpha))
    }
  
  def average(list: List[Int]): Int = list.sum / list.length
  
  def split(total: Int, numTasks: Int): List[(Int, Int)] = {
    val tasks = if (total < numTasks) total else numTasks
    var stripSize = total / tasks
    if (total % tasks > 0) stripSize += 1
    
    (for {
      stripIndex <- (0 to numTasks)
      start = stripIndex * stripSize
      next = start + stripSize
      end = if (next > total) total else next
      if (start < end)
    } yield (start, end)).toList
  }
  
  def blurStrips(strips: List[(Int, Int)], src: Img, dst: Img, radius: Int)
      (blur: (Img, Img, Int, Int, Int) => Unit): Unit =
    strips match {
      case Nil => ()
      case x1 :: Nil =>
        parallel(
          blur(src, dst, x1._1, x1._2, radius),
          ())
      case x1 :: x2 :: tail =>
        parallel(
          parallel(
            blur(src, dst, x1._1, x1._2, radius),
            blur(src, dst, x2._1, x2._2, radius)),
          blurStrips(tail, src, dst, radius)(blur)
        )
    }
}
