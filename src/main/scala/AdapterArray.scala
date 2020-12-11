object AdapterArray {
  def calculateCombinations(adapters: List[Int]):Long = {
    val adaptersOrdered = adapters.sorted
    val first = traverseArray(adaptersOrdered, (List(0),0,0,0))

    val res = first._1.reverse.tail.foldLeft((List[Long](0), List[Long]())) { (acc, n) =>
      if (acc._1.reverse.head + 4 >= n)
        (n :: acc._1, acc._2)
      else if (acc._1.size == 5)
        (List(n), 7 :: acc._2)
      else if (acc._1.size == 4)
        (List(n), 4 :: acc._2)
      else if (acc._1.size == 3 && acc._1.max - acc._1.min <= 3)
        (List(n), 2 :: acc._2)
      else
        (n :: acc._1.reverse.tail.reverse, acc._2)
    }
    val extra = if (res._1.size == 5)
      7L :: res._2
    else if (res._1.size == 4)
      4L :: res._2
    else if (res._1.size == 3 && res._1.max - res._1.min <= 3)
      2L :: res._2
    else
      res._2

    extra.product
  }

  def calculateCombinations2(adapters: List[Int]) = {
    val v = Vector(1L,1L,2L) ++ Vector.fill(adapters.max - 2)(0L)
    Range(3,adapters.max + 1).foldLeft(v) { (acc, n) =>
      if (adapters.contains(n))
        acc.updated(n,acc(n-3) + acc(n-2) + acc(n-1))
      else
        acc
    }.last
  }

  def traverseArray(adapters: List[Int], acc: (List[Int],Int,Int,Int)): (List[Int],Int,Int,Int) = {
    if (adapters.nonEmpty) {
      val next = adapters.head
      if (next == acc._1.head + 1)
        traverseArray(adapters.tail, (next :: acc._1, acc._2 + 1, acc._3, acc._4))
      else if (next == acc._1.head + 2)
        traverseArray(adapters.tail, (next :: acc._1, acc._2, acc._3 + 1, acc._4))
      else if (next == acc._1.head + 3)
        traverseArray(adapters.tail, (next :: acc._1, acc._2, acc._3, acc._4 + 1))
      else
        acc
    }
    else
      acc
  }

  def calculateJoltMultiplication(adapters: List[Int]) = {
    val adaptersOrdered = adapters.sorted
    val res = traverseArray(adaptersOrdered, (List(0),0,0,0))
    System.out.println(res._1)
    res._2 * (res._4 + 1)

  }
}
