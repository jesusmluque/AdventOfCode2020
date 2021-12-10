import org.scalatest.FlatSpec

import scala.io.Source

class DockingDataTest extends FlatSpec{

  "The sum of all values left in memory after it completes for the DockingDataExercise1.txt instructions file " should " be 165 " in {
    assert(DockingData.getTotalMemoryValue(Source.fromResource("DockingDataExercise1.txt").getLines()) == 165)
  }

  "The sum of all values left in memory after it completes for the DockingData.txt instructions file " should " be 9615006043476 " in {
    assert(DockingData.getTotalMemoryValue(Source.fromResource("DockingData.txt").getLines()) == 9615006043476L)
  }

  "The sum of all values left in memory after it completes for the DockingDataExercise2.txt instructions file and floating masks " should " be 208 " in {
    assert(DockingData.getTotalMemoryValueWithFloating(Source.fromResource("DockingDataExercise2.txt").getLines()) == 208)
  }

  "The sum of all values left in memory after it completes for the DockingDataExercise3.txt instructions file and floating masks " should " be 208 " in {
    assert(DockingData.getTotalMemoryValueWithFloating(Source.fromResource("DockingDataExercise3.txt").getLines()) == 608)
  }

  "The sum of all values left in memory after it completes for the DockingData.txt instructions file and floating masks " should " be 165 " in {
    assert(DockingData.getTotalMemoryValueWithFloating(Source.fromResource("DockingData.txt").getLines()) == 4275496544925L)
  }
}
