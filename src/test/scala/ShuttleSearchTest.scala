import org.scalatest.FlatSpec

import scala.io.Source

class ShuttleSearchTest extends FlatSpec {

  "The ID of the earliest bus you can take to the airport" +
    " multiplied by the number of minutes you'll need to wait for that bus (for ShuttleSearchExercise1.txt) " should " be 295 " in {
    assert(ShuttleSearch.calculateCloserBus(Source.fromResource("ShuttleSearchExercise1.txt").getLines().toList) == 295)
  }

  "The ID of the earliest bus you can take to the airport" +
    " multiplied by the number of minutes you'll need to wait for that bus (for ShuttleSearch.txt) " should " be 1915 " in {
    assert(ShuttleSearch.calculateCloserBus(Source.fromResource("ShuttleSearch.txt").getLines().toList) == 1915)
  }

  "The earliest timestamp such that all of the listed bus IDs depart " +
    "at offsets matching their positions in the list (ShuttleSearchExercise1.txt) " should " be 1068781" in {
      assert(ShuttleSearch.caculateOffset(Source.fromResource("ShuttleSearchExercise1.txt").getLines().toList.tail.head) == 1068781L)
    }

  "The earliest timestamp such that all of the listed bus IDs depart " +
    "at offsets matching their positions in the list (ShuttleSearchExercise2.txt) " should " be 3417" in {
    assert(ShuttleSearch.caculateOffset(Source.fromResource("ShuttleSearchExercise2.txt").getLines().toList.tail.head) == 3417L)
  }

  "The earliest timestamp such that all of the listed bus IDs depart " +
    "at offsets matching their positions in the list (ShuttleSearchExercise2.txt) " should " be 1202161486" in {
    assert(ShuttleSearch.caculateOffset(Source.fromResource("ShuttleSearchExercise3.txt").getLines().toList.tail.head) == 1202161486L)
  }

  "The earliest timestamp such that all of the listed bus IDs depart " +
    "at offsets matching their positions in the list (ShuttleSearch.txt) " should " be 294354277694107L" in {
    assert(ShuttleSearch.caculateOffset(Source.fromResource("ShuttleSearch.txt").getLines().toList.tail.head) == 294354277694107L)
  }
}
