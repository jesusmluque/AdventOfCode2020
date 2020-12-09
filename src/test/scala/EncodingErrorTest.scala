import org.scalatest.FlatSpec

import scala.io.Source

class EncodingErrorTest extends FlatSpec {

  "The first number that doesn't follow the rule in the EncodingExercise.txt " should " be 127 " in {
    assert(EncodingError.findFirstNumberWithErrorInto(Source.fromResource("EncodingExercise.txt").getLines().toList.map(_.toLong), 5) == 127)
  }

  "The first number that doesn't follow the rule in the Encoding.txt " should " be 127 " in {
    assert(EncodingError.findFirstNumberWithErrorInto(Source.fromResource("Encoding.txt").getLines().toList.map(_.toLong), 25) == 530627549L)
  }

  "The sum of the min and max numbers that form the subset that sum 127 for EncodingExercise.txt " should " be 62 " in {
    assert(EncodingError.findSum(Source.fromResource("EncodingExercise.txt").getLines().toList.map(_.toLong), 5) == 62)
  }

  "The sum of the min and max numbers that form the subset that sum 127 for Encoding.txt " should " be 77730285 " in {
    assert(EncodingError.findSum(Source.fromResource("Encoding.txt").getLines().toList.map(_.toLong), 25) == 77730285L)
  }

}
