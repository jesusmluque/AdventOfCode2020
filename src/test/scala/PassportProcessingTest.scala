import org.scalatest.FlatSpec

import scala.io.Source

class PassportProcessingTest extends FlatSpec {

  "When the file is the one in the example, the valid passports" should " be 2" in {
    assert(PassportProcessing.countPassports(Source.fromResource("Passports1.txt")) == 2)
  }

  "When the file is the target one, the valid passports" should " be 123" in {
    assert(PassportProcessing.countPassports(Source.fromResource("Passports2.txt")) == 123)
  }

  "When the file is only 2, the valid passports" should " be 1" in {
    assert(PassportProcessing.countPassports(Source.fromResource("Passports3.txt")) == 1)
  }

  "When the file is onl, the valid passports" should " be 4" in {
    assert(PassportProcessing.countPassports(Source.fromResource("Passports4.txt")) == 4)
  }

  "When the file only has invalid passports, the count " should " be 0" in {
    assert(PassportProcessing.countPassports(Source.fromResource("PassportsInvalids.txt")) == 0)
  }

  "When the file only has valid passports, the count " should " be 4" in {
    assert(PassportProcessing.countPassports(Source.fromResource("PassportsValid.txt")) == 4)
  }
}
