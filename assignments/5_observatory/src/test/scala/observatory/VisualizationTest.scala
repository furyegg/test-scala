package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {
  test("interpolateColor 1") {
    val points = List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255)))
    val temp = 0.25
    assert(Visualization.interpolateColor(points, temp) == Color(191, 0, 64))
  }
  
  test("calculate distance") {
    val knowLoc = Location(43.283, 20.8)
    val location = Location(90.0, -180.0)
    
    val dist = knowLoc.distanceTo(location)
    println(dist)
  }
  
}