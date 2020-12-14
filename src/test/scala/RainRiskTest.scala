import org.scalatest.FlatSpec

import scala.io.Source

class RainRiskTest extends FlatSpec {
  "The Manhattan distance between that location and the ship's starting position for the RainRiskExercise1.txt movements file " should " be 25 " in {
    assert(RainRisk.getDistance(Source.fromResource("RainRiskExercise1.txt").getLines().toList) == 25)
  }

  "The Manhattan distance between that location and the ship's starting position for the RainRisk.txt movements file " should " be 882 " in {
    assert(RainRisk.getDistance(Source.fromResource("RainRisk.txt").getLines().toList) == 882)
  }

  "Assuming the waypoint position, the Manhattan distance between that location and the ship's starting position for the RainRiskExercise1.txt movements file " should " be 25 " in {
    assert(RainRisk.getDistanceWithWaypoint(Source.fromResource("RainRiskExercise1.txt").getLines().toList) == 286)
  }

  "Assuming the waypoint position, the Manhattan distance between that location and the ship's starting position for the RainRiskExercise2.txt movements file " should " be 25 " in {
    assert(RainRisk.getDistanceWithWaypoint(Source.fromResource("RainRiskExercise2.txt").getLines().toList) == 344)
  }

  "Assuming the waypoint position, the Manhattan distance between that location and the ship's starting position for the RainRisk.txt movements file " should " be 25 " in {
    assert(RainRisk.getDistanceWithWaypoint(Source.fromResource("RainRisk.txt").getLines().toList) == 28885)
  }
}
