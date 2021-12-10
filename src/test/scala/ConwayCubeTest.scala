import org.scalatest.FlatSpec

import scala.io.Source

class ConwayCubeTest extends FlatSpec {

  "After 6 cycles, there " should " be 112 cubes left" in {
    assert(ConwayCube.getTotalCubesAfter(6, Source.fromResource("ConwayCube1.txt")) == 112)
  }
}
