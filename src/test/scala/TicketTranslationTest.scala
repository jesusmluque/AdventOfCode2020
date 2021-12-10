import org.scalatest.FlatSpec

import scala.io.Source

class TicketTranslationTest extends FlatSpec{

  "The ticket scanning error rate for the TicketTranslationExercise1.txt file " should " be 71 " in {
    assert(TicketTranslation.getScanningErrorRateFor(Source.fromResource("TicketTranslationExercise1.txt").getLines) == 71)
  }

  "The ticket scanning error rate for the TicketTranslation.txt file " should " be 71 " in {
    assert(TicketTranslation.getScanningErrorRateFor(Source.fromResource("TicketTranslation.txt").getLines) == 19070)
  }

  "The ticket departure fields multiplied them self for the TicketTranslation.txt file " should " be 161926544831 " in {
    assert(TicketTranslation.getDepartureMult(Source.fromResource("TicketTranslation.txt").getLines) == 161926544831L)
  }
}
