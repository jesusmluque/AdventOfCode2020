import scala.annotation.tailrec

object HandheldHalting {

  def parseRawInstructions(rawInstructions: Vector[String]) = rawInstructions.map { raw =>
      (raw.split(" ")(0), raw.split(" ")(1).toInt)
    }

  def executeWith(instructions: Vector[(String, Int)]) = {
    @tailrec
    def execute(instructions: Vector[(String, Int)], pointer: Int, acc: Int, instructionUsed: List[Int]):(Int, List[Int], Boolean) = {
      if (pointer >= instructions.size)
        (acc, instructionUsed, false)
      else
        instructions(pointer) match {
          case _ if instructionUsed.contains(pointer) => (acc, instructionUsed, true)
          case ("acc", value) => execute(instructions, pointer + 1, acc + value, pointer :: instructionUsed)
          case ("jmp", value) => execute(instructions, pointer + value, acc, pointer :: instructionUsed)
          case ("nop", _) => execute(instructions, pointer + 1, acc, pointer :: instructionUsed)
        }
    }
    execute(instructions, 0, 0, List())
  }

  def executeProgram(rawInstructions: Vector[String]) = executeWith(parseRawInstructions(rawInstructions))._1

  def modifyProgram(instructions: Vector[(String, Int)]) = {
    val (acc1, instructionList, isLoop) = executeWith(instructions)
    instructionList.map { indexIns =>
      instructions(indexIns) match {
        case ("nop", value)  => executeWith(instructions.updated(indexIns, ("jmp", value)))
        case ("jmp", value) => executeWith(instructions.updated(indexIns, ("nop", value)))
        case ("acc", _) => (0, List(), true)
      }
    }.find(!_._3).getOrElse((acc1, instructionList, isLoop))
  }

  def executeProgram2(rawInstructions: Vector[String]) =
    modifyProgram(parseRawInstructions(rawInstructions))._1
}
