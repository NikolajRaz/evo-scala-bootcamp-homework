package basics

import SecondHomework._
import basics.SecondHomework
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Gen._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.cats.implicits._

class SecondHomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks{
  "Triangle" should "be correct" in {
    val triangle = Triangle(Point(1, 2), Point(3, -1),Point(2, 5))
    triangle.maxX shouldEqual 3
    triangle.minX shouldEqual 1
    triangle.maxY shouldEqual 5
    triangle.minY shouldEqual -1
    triangle.area.toFloat shouldEqual 4.5
  }

  "Cube" should "be correct" in {
    val cube = Cube(Point3D(0, 0, 0), 10)
    cube.maxX shouldEqual 5
    cube.minX shouldEqual -5
    cube.maxY shouldEqual 5
    cube.minY shouldEqual -5
    cube.maxZ shouldEqual 5
    cube.minZ shouldEqual -5
    cube.surfaceArea.toFloat shouldEqual 600
    cube.volume.toFloat shouldEqual 1000
  }
}
