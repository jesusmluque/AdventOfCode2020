object TicketTranslation {

  def getDepartureMult(rawLines: Iterator[String]) = {
    val (rules, myTicket, tickets) = parseInput(rawLines)
    val validTickets = tickets.filter { ticket =>
      ticket.forall(verifyRules(_, rules) == 0)
    }
    val initVector: Vector[Set[String]] = Vector.fill(validTickets.head.size)(Set[(String)]())
    val ticketsRulesPositions = validTickets.foldLeft(initVector) { (acc, ticket) =>
      ticket.zipWithIndex.foldLeft(acc) { (v, num) =>
        val previous = acc(num._2)
        val rulesThatApply = if (previous.isEmpty)
          rules.filter(rule => (num._1 >= rule._2._1 && num._1 <= rule._2._2) || (num._1 >= rule._3._1 && num._1 <= rule._3._2)).map(_._1).toSet
        else
          previous.intersect(rules.filter(rule => (num._1 >= rule._2._1 && num._1 <= rule._2._2) || (num._1 >= rule._3._1 && num._1 <= rule._3._2)).map(_._1).toSet)

        v.updated(num._2, rulesThatApply)
      }
    }
    val fields = findElement(ticketsRulesPositions.toList)
    fields.filter( n => n._1.foldLeft(false)((a, n) => a || n.contains("departure"))).map(n => myTicket(n._2).toLong).product

  }

  def findElement(ticketsRulesPositions: List[Set[String]]) = {
    def find(ticketsRulesPositions: List[(Set[String], Int)], newList: List[(Set[String], Int)]):List[(Set[String], Int)] = ticketsRulesPositions match {
      case head :: List() => head :: newList
      case head :: second :: tail => find(second :: tail, (head._1.diff(second._1), head._2) :: newList)
      case List() => newList
    }
    find(ticketsRulesPositions.zipWithIndex.sortBy(_._1.size).reverse, List())
  }

  def verifyRules(num: Int, rules: List[(String, (Int, Int), (Int, Int))]): Int = {
    val isOk = rules.foldLeft(false)((a, p) =>
      a || ((num >= p._2._1 && num <= p._2._2) || (num >= p._3._1 && num <= p._3._2)))
    if (isOk)
      0
    else
      num
  }
  def getScanningErrorRateFor(rawLines: Iterator[String]) = {
    val (rules, _, tickets) = parseInput(rawLines)

    (for {
      ticket <- tickets
      num <- ticket
    } yield {
      verifyRules(num, rules)
    }).sum
  }

  private def parseInput(rawLines: Iterator[String]) = {
    val (rawRules, rawNearbyTickets) = rawLines.foldLeft((List[String](), List[String]())) { (acc, line) =>
      if (line.matches("""\D+: (\d+)-(\d+) or (\d+)-(\d+)""")) {
        (line :: acc._1, acc._2)
      } else if (line.matches("""(\d{1,3},)+(\d{1,3})""")) {
        (acc._1, line :: acc._2)
      } else
        acc
    }
    val rules = rawRules.map { next =>
      val List(klass, n1, n2, n3, n4) = """(\D+): (\d+)-(\d+) or (\d+)-(\d+)""".r.findAllIn(next).matchData.flatMap(_.subgroups).toList
      (klass, (n1.toInt, n2.toInt), (n3.toInt, n4.toInt))
    }
    val myTicket :: tickets = rawNearbyTickets.map { next =>
      """(\d{1,3})""".r.findAllIn(next).matchData.flatMap(_.subgroups).toList.map(_.toInt)
    }.reverse
    (rules, myTicket, tickets)
  }
}
