import scala.annotation.tailrec

object EncodingError {

  def findFirstNumberWithErrorInto(code: List[Long], preambleSize:Int) = {
    @tailrec
    def isNotCorrect(num: Long, preamble: List[Long], acc: Boolean): Boolean = preamble match {
        case _ :: List() => acc
        case head :: tail => isNotCorrect(num, tail, tail.foldLeft(acc) { (a, n) => if (head + n == num) false else a})
      }
    @tailrec
    def find(rest: List[Long], preamble: List[Long]): Long = rest match {
        case head :: List() => if (isNotCorrect(head, preamble, true)) head else -1
        case head :: tail => if (isNotCorrect(head, preamble, true)) head else find(tail, slideWindowAddingNewField(preamble.tail, head))
      }
    val preamble = code.take(preambleSize)
    val rest = code.takeRight(code.size - preambleSize)
    find(rest, preamble)
  }

  def findSum(code: List[Long], preambleSize:Int):Long = {
    @tailrec
    def find(num:Long, code: List[Long], acc: List[Long]):List[Long] =
      if (num - code.head - (if (acc.nonEmpty) acc.sum else 0) < 0)
        find(num, code, acc.tail)
      else if (num - code.head - acc.sum == 0)
        code.head :: acc
      else
        find(num, code.tail, slideWindowAddingNewField(acc, code.head))

    val num = findFirstNumberWithErrorInto(code, preambleSize)
    val codeParsed = code.takeWhile(_ <= num)
    val res = find(num, codeParsed, List()).sorted
    res.head + res.reverse.head
  }

  private def slideWindowAddingNewField(window: List[Long], field: Long) = (field :: window.reverse).reverse
}
