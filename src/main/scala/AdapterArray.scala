import scala.annotation.tailrec

object AdapterArray {

  def calculateCombinations(adapters: List[Int]):Long = {
    val (first, _, _, _) = traverseArray(adapters)
    System.out.println(first)
    val res = first.tail.foldLeft((List[Long](0), List[Long]())) { (acc, n) => {
      System.out.println((acc._1.reverse, acc._2))
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
    }
    System.out.println(res)
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
    val v = Vector(1L) ++ Vector.fill(adapters.max)(0L)
    val res = Range(1,adapters.max + 1).foldLeft(v) { (acc, n) =>
      if (adapters.contains(n)) {
        System.out.println(acc)
        acc.updated(n, (if (n - 3 < 0) 0 else acc(n - 3)) + (if (n - 2 < 0) 0 else acc(n - 2)) + (if (n - 1 < 0) 0 else acc(n - 1)))
      } else
        acc
    }
    System.out.println(res)
    res.last
  }
  def traverseArray(adapters: List[Int]) = {
    @tailrec
    def traverse(adapters: List[Int], acc: (List[Int],Int,Int,Int)): (List[Int],Int,Int,Int) = {
      if (adapters.nonEmpty) {
        val next = adapters.head
        if (next == acc._1.head + 1)
          traverse(adapters.tail, (next :: acc._1, acc._2 + 1, acc._3, acc._4))
        else if (next == acc._1.head + 2)
          traverse(adapters.tail, (next :: acc._1, acc._2, acc._3 + 1, acc._4))
        else if (next == acc._1.head + 3)
          traverse(adapters.tail, (next :: acc._1, acc._2, acc._3, acc._4 + 1))
        else
          (acc._1.reverse, acc._2, acc._3, acc._4)
      }
      else
        (acc._1.reverse, acc._2, acc._3, acc._4)
    }
    traverse(adapters.sorted, (List(0),0,0,0))
  }


  def calculateJoltMultiplication(adapters: List[Int]) = {
    val res = traverseArray(adapters)
    System.out.println(res._1)
    res._2 * (res._4 + 1)
  }
}
