import scala.io.BufferedSource

object ConwayCube {
  type Coordinates = (Int,Int,Int)
  type Cube[A] = Vector[Vector[Vector[(Coordinates, A)]]]
  case class CubeSpace[A](private val space: List[Coordinates]) {
    def getNeighbors(p: Coordinates) = List((p._1 + 1, p._2 + 1, p._3 + 1))
  }

  def getTotalCubesAfter(i: Int, source: BufferedSource) = ???



  def executeSixCycles(cubes: Vector[String]) = {
    val cycle = 1
    val initialSpace: CubeSpace = Vector(cubes.zipWithIndex.map { cube =>
      cube._1.split("").zipWithIndex.toVector.map(n => ((0, cube._2, n._2), n._1))
    })


  }
}
