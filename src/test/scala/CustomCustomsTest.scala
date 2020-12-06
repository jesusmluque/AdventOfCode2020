import org.scalatest.FlatSpec

import scala.io.Source

class CustomCustomsTest extends FlatSpec {

  "The number of answers yes in the input file AnswersExample " should "be 11" in {
    assert(CustomCustoms.totalAnswers(Source.fromResource("AnswersExample.txt")) == 11)
  }

  "The number of answers yes in the input file AnswersExercise " should "be 11" in {
    assert(CustomCustoms.totalAnswers(Source.fromResource("AnswersExercise.txt")) == 6534)
  }

  "The number of answers yes for all in each group in the input file AnswersExample " should "be 6" in {
    assert(CustomCustoms.totalAnswers2(Source.fromResource("AnswersExample.txt")) == 6)
  }

  "The number of answers yes for all in each group in the input file AnswersExercise " should "be 3402" in {
    assert(CustomCustoms.totalAnswers2(Source.fromResource("AnswersExercise.txt")) == 3402)
  }

}
