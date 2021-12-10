object DockingData {

  def getTotalMemoryValue(program: Iterator[String]): Long = {
    def applyMask(mask: String, value: Long) = {
      val binaryValue = convertLongToBinary(value, "").split("").toVector.reverse
      mask.split("").toVector.reverse.zipWithIndex.map { digit =>
        if (digit._1 == "X" && digit._2 < binaryValue.length)
          binaryValue(digit._2).toLong
        else if (digit._1 == "X" && digit._2 >= binaryValue.length)
          0L
        else if (digit._1 != "X")
          digit._1.toLong
        else
          binaryValue(digit._2).toLong

      }.reverse
    }

    program.foldLeft((Map[Long, Long](), "")) { (acc, next) => next match {
      case mask if mask.startsWith("mask = ") => (acc._1, mask.split("mask = ")(1))
      case memory if memory.startsWith("mem[") => {
        val List(add, value) = "(\\d+)".r.findAllIn(memory).toList

        (acc._1 + (add.toLong -> convertBinaryToLong(applyMask(acc._2, value.toLong))), acc._2)
      }
    }
    }._1.foldLeft(0L)((sum, n) => {
      sum + n._2
    })
  }

  def getTotalMemoryValueWithFloating(program: Iterator[String]) = {
    def applyFloatingMask(mask: String, value: Long):Vector[String] = {
      val binaryValue = convertLongToBinary(value, "").split("").toVector.reverse
      mask.split("").toVector.reverse.zipWithIndex.map { digit =>
        if (digit._1 == "X")
          "X"
        else if (digit._1 == "1")
          "1"
        else if (digit._2 < binaryValue.length)
          binaryValue(digit._2)
        else
          "0"
      }.filter(_ != "").reverse
    }
    def addressGen(maskedAdd: Vector[String]): List[Long] = {
      def modify(value: Vector[String], modification: Vector[(String,Int)]): Vector[String] = {
        if (modification.isEmpty)
          value
        else {
          val mod = modification.head
          val newValue = value.updated(mod._2, mod._1)
          modify(newValue, modification.tail)
        }
      }
      val xSubstitutions = maskedAdd.zipWithIndex.filter(_._1 == "X")
        .flatMap(a => Vector(("1", a._2), ("0", a._2)))
      val xs = maskedAdd.count(_ == "X")
      val comb = xSubstitutions.combinations(xs).toList.filter(a => a.map(_._2).distinct.length > xs - 1)

      comb.map(bin => convertBinaryToLong(modify(maskedAdd, bin).map(_.toLong)))
    }

    program.foldLeft((Map[Long, Long](), "")) { (acc, next) => next match {
      case mask if mask.startsWith("mask = ") => (acc._1, mask.split("mask = ")(1))
      case memory if memory.startsWith("mem[") => {
        val List(add, value) = "(\\d+)".r.findAllIn(memory)
          .toList
        val maskApplied = applyFloatingMask(acc._2, add.toLong)
        (addressGen(maskApplied).foldLeft(acc._1) { (addresses, n) =>
          addresses.updated(n, value.toLong)
        }, acc._2)
      }
    }
    }._1.foldLeft(0L)((sum, n) => {
      sum + n._2
    })
  }

  private def convertLongToBinary(value: Long, bin: String):String = value match {
    case 1L => "1" ++ bin
    case 0L => "0" ++ bin
    case n => convertLongToBinary(n / 2L, (n % 2).toString ++ bin)
  }
  private def convertBinaryToLong(value: Vector[Long]): Long = value.reverse.zipWithIndex.foldLeft(0L) { (acc, n) =>
    (acc.toDouble + n._1.toDouble * Math.pow(2D, n._2.toDouble)).toLong
  }

}
