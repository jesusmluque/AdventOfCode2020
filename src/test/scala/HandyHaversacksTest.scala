import org.scalatest.FlatSpec

import scala.io.Source

class HandyHaversacksTest extends FlatSpec{

  "The number of colors that contain at least one shiny gold bag in the BagsRulesExercise file " should "be 4" in {
    assert(HandyHaversacks.getTotalContainerConfigsForShinyGoldBug(Source.fromResource("BagsRulesExercise.txt").getLines().toList) == 4)
  }

  "The number of colors that contain at least one shiny gold bag in the BagsRules file " should "be 131" in {
    assert(HandyHaversacks.getTotalContainerConfigsForShinyGoldBug(Source.fromResource("BagsRules.txt").getLines().toList) == 131)
  }

  "The number of bags into the gold shiny bug in the BagsRulesExercise file " should "be 32" in {
    assert(HandyHaversacks.getTotalBagsForShinyGoldBug(Source.fromResource("BagsRulesExercise.txt").getLines().toList) == 32)
  }

  "The number of bags into the gold shiny bug in the BagsRulesExercise2 file " should "be 126" in {
    assert(HandyHaversacks.getTotalBagsForShinyGoldBug(Source.fromResource("BagsRulesExercise2.txt").getLines().toList) == 126)
  }

  "The number of bags into the gold shiny bug in the BagsRules file " should "be 11261" in {
    assert(HandyHaversacks.getTotalBagsForShinyGoldBug(Source.fromResource("BagsRules.txt").getLines().toList) == 11261)
  }
}
