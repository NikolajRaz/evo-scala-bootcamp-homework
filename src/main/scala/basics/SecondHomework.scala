package basics

object SecondHomework {
  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  sealed trait Shape2D[A] extends Located2D with Bounded2D with Movable2D[A] with Area

  sealed trait Shape3D[A] extends Located3D with Bounded3D with Movable3D[A] with Characteristics

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable2D[A] {
    def move(dx: Double, dy: Double): A
  }

  sealed trait Movable3D[A] {
    def move(dx: Double, dy: Double, dz: Double): A
  }

  sealed trait Area {
    def area: Double
  }

  sealed trait Characteristics {
    def surfaceArea: Double
    def volume: Double
  }

  final case class Point(x: Double, y: Double) extends Shape2D[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def area: Double = 0
    override def move(dx:Double, dy: Double): Point = Point(x+dx, y+dy)
  }

  final case class Triangle(firstPoint: Point, secondPoint: Point, thirdPoint: Point) extends Shape2D[Triangle] {
    override def x: Double = ???
    override def y: Double = ???
    override def minX: Double = Set(firstPoint.x, secondPoint.x, thirdPoint.x).min
    override def maxX: Double = Set(firstPoint.x, secondPoint.x, thirdPoint.x).max
    override def minY: Double = Set(firstPoint.y, secondPoint.y, thirdPoint.y).min
    override def maxY: Double = Set(firstPoint.y, secondPoint.y, thirdPoint.y).max
    override def area: Double = math.sqrt(semiPerimeter*(semiPerimeter-firstSide)*(semiPerimeter-secondSide)*(semiPerimeter-thirdSide))
    override def move(dx:Double, dy: Double): Triangle = Triangle(Point(firstPoint.x+dx, firstPoint.y+dy),Point(secondPoint.x+dx, secondPoint.y+dy), Point(thirdPoint.x+dx, thirdPoint.y+dy))

    def firstSide: Double = sideLength(firstPoint.x, firstPoint.y, secondPoint.x, secondPoint.y)
    def secondSide: Double = sideLength(firstPoint.x, firstPoint.y, thirdPoint.x, thirdPoint.y)
    def thirdSide: Double = sideLength(secondPoint.x, secondPoint.y, thirdPoint.x, thirdPoint.y)
    def semiPerimeter: Double = (firstSide + secondSide + thirdSide)/2
    def sideLength(x1:Double, y1:Double, x2:Double, y2:Double):Double = Math.sqrt(Math.pow((x2-x1), 2) + Math.pow((y2-y1), 2))
  }

  final case class Square(x:Double, y:Double, sideLength: Double) extends Shape2D[Square] {
    override def minX: Double = x-(sideLength/2)
    override def maxX: Double = x+(sideLength/2)
    override def minY: Double = y-(sideLength/2)
    override def maxY: Double = y+(sideLength/2)
    override def move(dx:Double, dy:Double): Square = Square(x+dx, y+dy, sideLength)
    override def area: Double = sideLength*sideLength
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D[Point3D] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def move(dx:Double, dy: Double, dz: Double): Point3D = Point3D(x+dx, y+dy, z+dz)
  }

  final case class Sphere(center: Point3D, radius: Double) extends Shape3D[Sphere] {
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
    override def minX: Double = x-radius
    override def maxX: Double = x+radius
    override def minY: Double = y-radius
    override def maxY: Double = y+radius
    override def minZ: Double = z+radius
    override def maxZ: Double = z+radius
    override def surfaceArea: Double = 4*3.14*Math.pow(radius, 2)
    override def volume: Double = 4/3*3.14*Math.pow(radius,3)
    override def move(dx:Double, dy:Double, dz: Double): Sphere = Sphere(Point3D(x+dx, y+dy, z+dz), radius)
  }

  final case class Cube(center: Point3D, sideLength: Double) extends Shape3D[Cube] {
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
    override def minX: Double = x-(sideLength/2)
    override def maxX: Double = x+(sideLength/2)
    override def minY: Double = y-(sideLength/2)
    override def maxY: Double = y+(sideLength/2)
    override def minZ: Double = z-(sideLength/2)
    override def maxZ: Double = z+(sideLength/2)
    override def surfaceArea: Double = 6*Math.pow(sideLength, 2)
    override def volume: Double = Math.pow(sideLength, 3)
    override def move(dx:Double, dy:Double, dz:Double): Cube = Cube(Point3D(x+dx, y+dy, z+dz), sideLength)
  }

  final case class Cuboid(center: Point3D, length:Double, width: Double, height:Double) extends Shape3D[Cuboid] {
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
    override def minX: Double = x-(width/2)
    override def maxX: Double = x+(width/2)
    override def minY: Double = y-(length/2)
    override def maxY: Double = y+(length/2)
    override def minZ: Double = z-(height/2)
    override def maxZ: Double = z+(height/2)
    override def surfaceArea: Double = 2 * (length*width+width*height+length*height)
    override def volume: Double = length*width*height
    override def move(dx:Double, dy:Double, dz:Double): Cuboid = Cuboid(Point3D(x+dx, y+dy, z+dz), length, width, height)
  }

  //getting coordinates of tetrahedron base triangle and it's apex
  final case class Triangle3D(firstPoint: Point3D, secondPoint: Point3D, thirdPoint: Point3D, apex: Point3D) extends Shape3D[Triangle3D] {
    override def z: Double = apex.z
    override def x: Double = apex.x
    override def y: Double = apex.y
    override def minX: Double = Set(firstPoint.x, secondPoint.x, thirdPoint.x, apex.x).min
    override def maxX: Double = Set(firstPoint.x, secondPoint.x, thirdPoint.x, apex.x).max
    override def minY: Double = Set(firstPoint.y, secondPoint.y, thirdPoint.y, apex.y).min
    override def maxY: Double = Set(firstPoint.y, secondPoint.y, thirdPoint.y, apex.y).max
    override def minZ: Double = Set(firstPoint.z, secondPoint.z, thirdPoint.z, apex.z).min
    override def maxZ: Double = Set(firstPoint.z, secondPoint.z, thirdPoint.z, apex.z).max
    override def surfaceArea: Double = ???
    override def volume: Double = 1/3*math.sqrt(baseSemiPerimeter*(baseSemiPerimeter-firstBaseSide)*(baseSemiPerimeter-secondBaseSide)*(baseSemiPerimeter-thirdBaseSide))*height
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D = Triangle3D(Point3D(firstPoint.x+dx, firstPoint.y + dy, firstPoint.z+dz),Point3D(secondPoint.x+dx, secondPoint.y+dy, secondPoint.z+dz), Point3D(thirdPoint.x+dx, thirdPoint.y+dy, thirdPoint.z+dz), Point3D(x+dx, y+dy, z+dz))

    def height: Double = ???
    def firstBaseSide: Double = sideLength(firstPoint.x, firstPoint.y, firstPoint.z , secondPoint.x, secondPoint.y, secondPoint.z)
    def secondBaseSide: Double = sideLength(firstPoint.x, firstPoint.y, firstPoint.z, thirdPoint.x, thirdPoint.y, thirdPoint.z)
    def thirdBaseSide: Double = sideLength(secondPoint.x, secondPoint.y, secondPoint.z, thirdPoint.x, thirdPoint.y, thirdPoint.z)
    def baseSemiPerimeter: Double = (firstBaseSide + secondBaseSide + thirdBaseSide)/2
    def sideLength(x1:Double, y1:Double, x2:Double, y2:Double, z1:Double, z2:Double):Double = Math.sqrt(Math.pow((x2-x1), 2) + Math.pow((y2-y1), 2) + Math.pow(z2,z1))
  }
}
