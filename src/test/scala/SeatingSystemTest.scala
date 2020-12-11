import org.scalatest.FlatSpec

import scala.io.Source

class SeatingSystemTest extends FlatSpec{

  "The seats occupied after the stabilization for the SeatingSystemExercise1.txt " should " be 37 " in {
    assert(SeatingSystem.calculateFinalSeats(Source.fromResource("SeatingSystemExercise1.txt").getLines().toVector.map(_.split("").toVector)) == 37)
  }

  "The seats occupied after the stabilization for the SeatingSystem.txt " should " be 2438 " in {
    assert(SeatingSystem.calculateFinalSeats(Source.fromResource("SeatingSystem.txt").getLines().toVector.map(_.split("").toVector)) == 2438)
  }

  "The seats occupied after the stabilization for the SeatingSystemExercise1.txt with the new rules " should " be 26 " in {
    assert(SeatingSystem.calculateFinalSeatsWithDeepSearch(Source.fromResource("SeatingSystemExercise1.txt").getLines().toVector.map(_.split("").toVector)) == 26)
  }

  "The seats occupied after the stabilization for the SeatingSystem.txt with the new rules " should " be 2174 " in {
    assert(SeatingSystem.calculateFinalSeatsWithDeepSearch(Source.fromResource("SeatingSystem.txt").getLines().toVector.map(_.split("").toVector)) == 2174)
  }
}
