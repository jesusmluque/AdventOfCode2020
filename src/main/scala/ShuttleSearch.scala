object ShuttleSearch {

  def calculateCloserBus(notes: List[String]) = {
    def calculate(busIds: List[Int], closerBus:(Int,Int), estimationTime: Int):(Int,Int) = {
      if (busIds.isEmpty)
        (closerBus._1, closerBus._2)
      else {
        val timeAtPortForID = (estimationTime.toDouble / busIds.head.toDouble).toInt * busIds.head
        val delta = (if (timeAtPortForID < estimationTime) timeAtPortForID + busIds.head else timeAtPortForID) - estimationTime
        calculate(busIds.tail, if (delta < closerBus._2) (busIds.head, delta) else closerBus, estimationTime)
      }
    }
    val departureEstimation = notes.head.toInt
    val busIds = notes.tail.head.split(",").toList.filter(_ != "x").map(_.toInt)

    val res = calculate(busIds, (Int.MaxValue, Int.MaxValue), departureEstimation)
    res._1 * res._2
  }
  def caculateOffset(notes: String): Long = {
    def calculate(busIds: List[(Long, Long)], start: Long, step: Long):Long = {
      if (busIds.isEmpty)
        start - step
      else {
        val (nrs, nextStep) = busIds.foldLeft((List[(Long,Long)](), step:Long))( (acc, next) => {
          if (start % next._1 == next._2) {
            (next :: acc._1, acc._2 * next._1)
          } else {
            acc
          }
        })
        calculate(busIds.filter(!nrs.contains(_)), start + nextStep, nextStep )
      }
    }
    val busIds = notes.split(",").zipWithIndex.toList.filter(_._1 != "x")
      .map(n => (n._1.toLong, n._2)).map { n =>
        val wait = n._1 - n._2 % n._1
        (n._1, wait % n._1)
      }
    calculate(busIds.tail, busIds.head._1, busIds.head._1)
  }
}
