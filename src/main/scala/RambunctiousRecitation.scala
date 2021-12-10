import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object RambunctiousRecitation {
  def get2020th(raw: List[Int], targetNum: Int): Int = {
    @tailrec
    def getNumber(nums: List[Int], occurrences: Map[Int, List[Int]], pointer: Int, first: Boolean):Int = {
       if (pointer >= targetNum) {
        nums.head
      } else
        if (!first && occurrences.contains(nums.head)) {
          val previous = occurrences.getOrElse(nums.head, List())
          val next = if (previous.size < 2) previous.head else previous.head - previous.tail.head
          val oldOcc = occurrences.getOrElse(next, List()).take(2)
          val newOcc = occurrences + ((next, (pointer + 1) :: oldOcc))
          getNumber(next :: nums, newOcc, pointer + 1, oldOcc.isEmpty)
        } else {
          val oldOcc = occurrences.getOrElse(0, List[Int]()).take(2)
          getNumber(0 :: nums, occurrences + ((0, (pointer + 1) :: oldOcc)), pointer + 1, oldOcc.isEmpty)
        }
    }
    getNumber(raw.reverse, raw.zipWithIndex.foldLeft(HashMap[Int, List[Int]]())((acc, n) => acc.updated(n._1, List(n._2 + 1))), raw.size, true)
  }
}
