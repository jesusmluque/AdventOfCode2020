
"FBFBBFFRLR".splitAt(7)
"0101100".split("").zipWithIndex.map { n =>
  n._1.toInt * Math.pow(2,n._2.toDouble)
}.sum
"111".split("").zipWithIndex.map { n =>
  n._1.toInt * Math.pow(2,n._2.toDouble)
}.sum

