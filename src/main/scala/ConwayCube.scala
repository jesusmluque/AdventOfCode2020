object ConwayCube {

  type CubeSpace = Vector[Vector[Vector[((Int,Int,Int), String)]]]

  def executeSixCycles(cubes: Vector[String]) = {
    val cycle = 1
    val initialSpace: CubeSpace = Vector(cubes.zipWithIndex.map { cube =>
      cube._1.split("").zipWithIndex.toVector.map(n => ((0, cube._2, n._2), n._1))
    })


  }
}
