import org.scalatest.FlatSpec

import scala.io.Source

class AdaptersArrayTest extends FlatSpec{

  "The number of 1-jolt differences multiplied by the number of 3-jolt when the file is AdaptersExercise1.txt " should " be " in {
    assert(AdapterArray.calculateJoltMultiplication(Source.fromResource("AdaptersExercise1.txt").getLines().toList.map(_.toInt)) == 35)
  }

  "The number of 1-jolt differences multiplied by the number of 3-jolt when the file is AdaptersExercise2.txt " should " be " in {
    assert(AdapterArray.calculateJoltMultiplication(Source.fromResource("AdaptersExercise2.txt").getLines().toList.map(_.toInt)) == 220)
  }

  "The number of 1-jolt differences multiplied by the number of 3-jolt when the file is Adapters.txt " should " be " in {
    assert(AdapterArray.calculateJoltMultiplication(Source.fromResource("Adapters.txt").getLines().toList.map(_.toInt)) == 1920)
  }

  "The total number of distinct ways you can arrange the adapters to connect the charging outlet to your device for the AdaptersExercise1.txt " should " be " in {
    assert(AdapterArray.calculateCombinations2(Source.fromResource("AdaptersExercise1.txt").getLines.toList.map(_.toInt)) == 8)
  }

  "The total number of distinct ways you can arrange the adapters to connect the charging outlet to your device for the AdaptersExercise2.txt " should " be " in {
    assert(AdapterArray.calculateCombinations2(Source.fromResource("AdaptersExercise2.txt").getLines.toList.map(_.toInt)) == 19208)
  }

  "The total number of distinct ways you can arrange the adapters to connect the charging outlet to your device for the Adapters.txt " should " be " in {
    assert(AdapterArray.calculateCombinations(Source.fromResource("Adapters.txt").getLines.toList.map(_.toInt)) == 1511207993344L)
  }

  "The total number of distinct ways you can arrange the adapters to connect the charging outlet to your device for the Adapters.txt and method 2 " should " be " in {
    assert(AdapterArray.calculateCombinations2(Source.fromResource("Adapters.txt").getLines.toList.map(_.toInt)) == 1511207993344L)
  }
}
