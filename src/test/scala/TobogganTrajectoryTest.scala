import org.scalatest.FlatSpec

import scala.io.Source

class TobogganTrajectoryTest extends FlatSpec {

  "The trajectory for the example map" should " encounter with 7 trees " in {
    val field = TobogganTrajectory.readField(Source.fromResource("FieldForTrajectory1.txt").getLines())
    assert(TobogganTrajectory.calculateTrajectory(field, 1, 3) == 7)
  }

  "The trajectory for the example2 map" should " encounter with 7 trees " in {
    val field = TobogganTrajectory.readField(Source.fromResource("FieldForTrajectory2.txt").getLines())
    assert(TobogganTrajectory.calculateTrajectory(field, 1, 3) == 176)
  }

  "The min trajectory for the example map" should " encounter with 336 trees " in {
    val field = TobogganTrajectory.readField(Source.fromResource("FieldForTrajectory1.txt").getLines())
    assert(TobogganTrajectory.bestPath(field) == 336)
  }

  "The min trajectory for the example2 map" should " encounter with 336 trees " in {
    val field = TobogganTrajectory.readField(Source.fromResource("FieldForTrajectory2.txt").getLines())
    assert(TobogganTrajectory.bestPath(field) == 5872458240L)
  }
}
