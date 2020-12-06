import scala.io.BufferedSource

object CustomCustoms {

  def totalAnswers(file: BufferedSource) = {
    file.getLines().foldLeft((0, Set[String]())) { (acc, n) =>
      val individualAnswers = n.split("")
      if (n.length == 0 )
        (acc._1, Set[String]())
      else
        (individualAnswers.count(x => !acc._2.contains(x)) + acc._1,individualAnswers.foldLeft(acc._2) { (s, next) =>
          s + next
        })

    }._1
  }

  def totalAnswers2(file: BufferedSource) = {
    def commonAnswersIn(answersByPerson: List[String]) =
      answersByPerson.head.split("").foldLeft(0){(t,e) =>
        if (answersByPerson.tail.forall(_.contains(e))) t + 1 else t
      }

    val res = file.getLines().foldLeft((0, List[String]())) { (acc, n) =>
      if (n.length == 0) {
        (acc._1 + commonAnswersIn(acc._2), List())
      } else
        (acc._1, n :: acc._2)
    }

    if (res._2.nonEmpty)
      res._1 + commonAnswersIn(res._2)
    else
      res._1
  }
}
