import org.scalatest.FlatSpec

import scala.io.Source

class BinaryBoardingTest extends FlatSpec {

  "The seat ID for the code FBFBBFFRLR " should "be 357" in {
    assert(BinaryBoarding.getID("FBFBBFFRLR") == 357)
  }

  "The higher ID for the codes into BinaryBoarding1.txt " should "be 820" in {
    assert(BinaryBoarding.getMaxID(Source.fromResource("BinaryBoarding1.txt").getLines().toList) == 820)
  }

  "The higher ID for the codes into BinaryBoarding2.txt " should "be 820" in {
    assert(BinaryBoarding.getMaxID(Source.fromResource("BinaryBoarding2.txt").getLines().toList) == 842)
  }

  "The seat ID in the BinaryBoarding2.txt " should "be 617" in {
    assert(BinaryBoarding.findTheSeat(Source.fromResource("BinaryBoarding2.txt").getLines().toList) == 617)
  }
}
