object TobogganTrajectory {

  def readField(lines: Iterator[String]) = lines.map { line =>
      line.split("").toVector
    }.toVector

  def calculateTrajectory(field: Vector[Vector[String]], down: Int, right: Int) = {
    val ysize = field.size
    val xsize = field.head.size
    field.foldLeft(List[(Int,Int,String)]()) { (acc, _) =>
      val position = if (acc.nonEmpty) (acc.head._1 + down, (acc.head._2  + right) % xsize) else (down,right)
      if (position._1 < ysize) field(position._1)(position._2) match {
          case "#" => (position._1, position._2, "X") :: acc
          case _   => (position._1, position._2, "O") :: acc
        } else acc
    }.count(_._3 == "X")
  }

  def bestPath(field: Vector[Vector[String]]) = {
    val options = List((1,1), (1,3), (1,5), (1, 7), (2, 1))
    options.map(op => calculateTrajectory(field, op._1, op._2).toLong).product
  }
}
