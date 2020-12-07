object HandyHaversacks {
  def getTotalContainerConfigsForShinyGoldBug(bagsRules: List[String]):Int = {
    def findTotalContainers(rules: Map[String, String], targetColor:String, total: Set[(String,String)]):Set[(String,String)] = {
      rules.filter(elem => elem._2.contains(targetColor)).foldLeft(total){(acc, elem) =>
        findTotalContainers(rules, elem._1.split(" bag")(0), acc + elem)
      }
    }
    val mapOfRules = bagsRules.foldLeft(Map[String, String]()) { (rules, rawRule) =>
      val rule = rawRule.split(" contain ")
      rules + (rule(0) -> rule(1))
    }
    findTotalContainers(mapOfRules, "shiny gold bag", Set()).size

  }

  def getTotalBagsForShinyGoldBug(bagsRules: List[String]):Int = {
    def findTotalInnerBugs(rules: Map[String, List[String]], bag:String):Int = {
      rules(bag).foldLeft(0) { (acc, elem) =>
        if (elem.contains("no other bag"))
          0
        else {
          val num = """\d{1}""".r.findAllIn(elem).toList.map(_.toInt).sum
          acc + num + num * findTotalInnerBugs(rules, elem.splitAt(2)._2.split(" bag")(0))
        }
      }

    }
    val mapOfRules = bagsRules.foldLeft(Map[String, List[String]]()) { (rules, rawRule) =>
      val rule = rawRule.split(" contain ")
      rules + (rule(0).split(" bag")(0) -> rule(1).split(", ").toList)
    }
    findTotalInnerBugs(mapOfRules, "shiny gold")
  }
}
