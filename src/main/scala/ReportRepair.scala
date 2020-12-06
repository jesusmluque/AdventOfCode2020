import scala.annotation.tailrec

object ReportRepair {
  @tailrec
  def findSum(a: List[Int], total: Int):Option[Int] = a.tail.foldLeft[(Int, Option[Int])]((a.head, None)) { (a, n) => a match {
      case (x, None) if x + n == total => (x, Some(x * n))
      case t => t
    }
    } match {
      case (_, None) if (a.isEmpty || a.tail.isEmpty) => None
      case (_, Some(x)) => Some(x)
      case (_, None) => findSum(a.tail, total)
    }

  @tailrec
  def findSum2(a: List[Int]):Option[Int] = {
    findSum(a.tail, 2020 - a.head) match {
      case None => findSum2(a.tail)
      case Some(r) => Some(r * a.head)
    }

  }

}
