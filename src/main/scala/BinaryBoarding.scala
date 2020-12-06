object BinaryBoarding {

  def getID(code: String) = {
    def calculateNumber(subCode: String) = {
      subCode.split("").reverse.map {
        case "F" => 0
        case "B" => 1
        case "L" => 0
        case "R" => 1
      }.zipWithIndex.map { n =>
        n._1.toDouble * Math.pow(2D,n._2.toDouble)
      }.sum.toInt
    }
    val subcodes = code.splitAt(7)
    val row = calculateNumber(subcodes._1)
    val col = calculateNumber(subcodes._2)
    row * 8 + col
  }

  def getMaxID(codes: List[String]) = {
    val orderedCodes = codes.map(getID).sorted

    orderedCodes.max
  }

  def findTheSeat(codes: List[String]) = {
    val orderedCodes = codes.map(getID).sorted
    Range(orderedCodes.head,orderedCodes.last).toSet.diff(orderedCodes.toSet).head
  }

}
