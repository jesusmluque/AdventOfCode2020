object RainRisk {

  def getDistance(instructions: List[String]) = {
    val initPosition = ((0,0), "east")
    val finalPos = instructions.foldLeft(initPosition) { (acc, next) =>
      calculateNextPosition(acc, next.splitAt(1))
    }
    (if (finalPos._1._1 < 0) -1 * finalPos._1._1 else finalPos._1._1) +
      (if (finalPos._1._2 < 0) -1 * finalPos._1._2 else finalPos._1._2)
  }

  def getDistanceWithWaypoint(instructions: List[String]) = {
    val initPositionForShip = ((0,0), "east")
    val initPositionForWaypoint = ((10,1), "east")
    val finalPos = instructions.foldLeft((initPositionForShip, initPositionForWaypoint)) { (acc, next) =>
      next.splitAt(1) match {
        case ("F", value) => {
          val newPostitionForShip = ((acc._2._1._1 * value.toInt + acc._1._1._1, acc._2._1._2 * value.toInt + acc._1._1._2), acc._1._2)
          (newPostitionForShip, acc._2)
        }
        case n @ ("N" | "S" | "E" | "W", _) => (acc._1, calculateNextPosition(acc._2,n))
        case ("R",  value) => {
          if (value.toInt == 90)
            (acc._1, ((acc._2._1._2, -1 * acc._2._1._1), acc._2._2))
          else if (value.toInt == 180)
            (acc._1, ((-1 * acc._2._1._1, -1 * acc._2._1._2), acc._2._2))
          else if (value.toInt == 270)
            (acc._1, ((-1 * acc._2._1._2, acc._2._1._1), acc._2._2))
          else
            acc
        }
        case ("L",  value) => {
          if (value.toInt == 90)
            (acc._1, (( -1 * acc._2._1._2, acc._2._1._1), acc._2._2))
          else if (value.toInt == 180)
            (acc._1, ((-1 * acc._2._1._1, -1 * acc._2._1._2), acc._2._2))
          else if (value.toInt == 270)
            (acc._1, ((acc._2._1._2, -1 * acc._2._1._1), acc._2._2))
          else
            acc
        }
      }
    }
    (if (finalPos._1._1._1 < 0) -1 * finalPos._1._1._1 else finalPos._1._1._1) +
      (if (finalPos._1._1._2 < 0) -1 * finalPos._1._1._2 else finalPos._1._1._2)
  }
  


  private def calculateNextPosition(acc: ((Int, Int), String), next: (String, String)) = {
    next match {
      case ("F", value) if acc._2 == "east" => ((acc._1._1 + value.toInt, acc._1._2), acc._2)
      case ("F", value) if acc._2 == "west" => ((acc._1._1 - value.toInt, acc._1._2), acc._2)
      case ("F", value) if acc._2 == "north" => ((acc._1._1, acc._1._2 + value.toInt), acc._2)
      case ("F", value) if acc._2 == "south" => ((acc._1._1, acc._1._2 - value.toInt), acc._2)
      case ("N", value) => ((acc._1._1, acc._1._2 + value.toInt), acc._2)
      case ("S", value) => ((acc._1._1, acc._1._2 - value.toInt), acc._2)
      case ("W", value) => ((acc._1._1 - value.toInt, acc._1._2), acc._2)
      case ("E", value) => ((acc._1._1 + value.toInt, acc._1._2), acc._2)
      case ("R", value) => (acc._1, fromDegreeToCardinalPointsRight(value.toInt, acc._2))
      case ("L", value) => (acc._1, fromDegreeToCardinalPointsLeft(value.toInt, acc._2))
    }
  }

  def fromDegreeToCardinalPointsRight(degrees: Int, previousCardinalPoint: String) = {
    val card = Vector("east", "south", "west", "north")
    card(((degrees / 90) + card.indexOf(previousCardinalPoint)) % 4)
  }

  def fromDegreeToCardinalPointsLeft(degrees: Int, previousCardinalPoint: String) = {
    val card = Vector("east", "north", "west", "south")
    card(((degrees / 90) + card.indexOf(previousCardinalPoint)) % 4)
  }
}
