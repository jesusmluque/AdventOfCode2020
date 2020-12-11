object SeatingSystem {

  def calculateFinalSeats(rawSeats: Vector[Vector[String]]): Int =
    stabilization(parseSeats(rawSeats), 3, false).map(_.count(a => a._2 == "#")).sum

  def calculateFinalSeatsWithDeepSearch(rawSeats: Vector[Vector[String]]): Int =
    stabilization(parseSeats(rawSeats), 4, true).map(_.count(a => a._2 == "#")).sum

  def stabilization(seats: Vector[Vector[((Int,Int), String)]], seatsAroundLimit:Int, withDeepSearch: Boolean): Vector[Vector[((Int,Int), String)]] = {
    val next = calculateRound(seats, seatsAroundLimit, withDeepSearch)
    if (next == seats)
      next
    else
      stabilization(next, seatsAroundLimit, withDeepSearch)
  }

  def calculateRound(seats: Vector[Vector[((Int,Int), String)]], seatsAroundLimit: Int, withDeepSearch: Boolean) =
    seats.map { raw =>
      raw.map { seat =>
        val occupied = getNeighbors(seats, seat._1, withDeepSearch).count(_ == "#")
        if (occupied == 0 && seat._2 == "L")
          (seat._1, "#")
        else if (occupied > seatsAroundLimit && seat._2 == "#")
          (seat._1, "L")
        else
          seat
      }
    }

  def getNeighbors(seats: Vector[Vector[((Int,Int), String)]], position: (Int,Int), withDeepSearch: Boolean) = {
    def sum(p1: (Int,Int), p2: (Int,Int)) = (p1._1 + p2._1, p1._2 + p2._2)
    def find(input: Vector[Vector[((Int,Int), String)]], pos: (Int,Int), delta: (Int,Int)): String = {
      val next = sum(pos, delta)
      if (next._1 < 0 || next._2 < 0 || input.size <= next._1 || input.head.size <= next._2)
        "."
      else if (input(next._1)(next._2)._2 != ".")
        input(next._1)(next._2)._2
      else if (withDeepSearch)
        find(input, next, delta)
      else
        input(next._1)(next._2)._2
    }
    List((0,-1), (-1,-1), (-1,0), (-1,1), (0,1), (1,1), (1,0), (1,-1)).map(find(seats,position,_))
  }

  private def parseSeats(rawSeats: Vector[Vector[String]]) = 
    rawSeats.zipWithIndex.map { row =>
      row._1.zipWithIndex.map { seat =>
        ((row._2, seat._2), seat._1)
      }
    }

}

