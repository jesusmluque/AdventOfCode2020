import org.scalatest.FlatSpec

import scala.io.Source

class HandheldHaltingTest extends FlatSpec {

  "The value of the accumulator for the InstructionsExample1.txt " should "be 5 " in {
    assert(HandheldHalting.executeProgram(Source.fromResource("InstructionsExample1.txt").getLines().toVector) == 5)
  }

  "The value of the accumulator for the Instructions.txt " should "be 2003 " in {
    assert(HandheldHalting.executeProgram(Source.fromResource("Instructions.txt").getLines().toVector) == 2003)
  }

  "The value of the accumulator when the program terminate for the InstructionsExample1.txt " should "be 8 " in {
    assert(HandheldHalting.executeProgram2(Source.fromResource("InstructionsExample1.txt").getLines().toVector) == 8)
  }

  "The value of the accumulator when the program terminate for the Instructions.txt " should "be 1984 " in {
    assert(HandheldHalting.executeProgram2(Source.fromResource("Instructions.txt").getLines().toVector) == 1984)
  }
}
