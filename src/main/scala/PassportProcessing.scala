import scala.io.BufferedSource

object PassportProcessing {

  case class Passport private (byr:String, iyr:String, eyr:String, hgt:String, hcl:String, ecl:String, pid:String, cid:Option[String])
  object Passport {
    def apply(raw: List[String]):Option[Passport] = {
      val rawData:Map[String, String] = raw.map(pair => pair.split(":").toList)
          .map(s => (s.head, s.tail.head)).toMap[String,String].withDefaultValue("")
      if (validate(rawData))
        Some(Passport(
          rawData("byr"),
          rawData("iyr"),
          rawData("eyr"),
          rawData("hgt"),
          rawData("hcl"),
          rawData("ecl"),
          rawData("pid"),
          if (rawData("cid") == "") None else Some(rawData("cid"))
        ))
      else
        None
    }

    private def validate(raw: Map[String, String]) = {

      val byr = raw("byr")
      val iyr = raw("iyr")
      val eyr = raw("eyr")
      val hgt = raw("hgt")
      val hcl = raw("hcl")
      val ecl = raw("ecl")
      val pid = raw("pid")

      (byr.matches("\\d{4}") && byr.toInt >= 1920 && byr.toInt <= 2002) &&
        (iyr.matches("\\d{4}") && iyr.toInt >= 2010 && iyr.toInt <= 2020) &&
        (eyr.matches("\\d{4}") && eyr.toInt >= 2020 && eyr.toInt <= 2030) &&
        ((hgt.matches("\\d{3}cm") && """\d{3}""".r.findAllIn(hgt).next().toInt >= 150 && """\d{3}""".r.findAllIn(hgt).next().toInt <= 193)
          || (hgt.matches("\\d{2}in") && """\d{2}""".r.findAllIn(hgt).next().toInt >= 59 && """\d{2}""".r.findAllIn(hgt).next().toInt <= 76)) &&
        hcl.matches("#([a-f,0-9]{6})") &&
        (List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")contains(ecl)) &&
        pid.matches("\\d{9}")
    }
  }

  private def readFile(file: BufferedSource) = {
    val raw = file.getLines().foldLeft((List[String](), List[Option[Passport]]())) { (acc, p) =>
      if (p.length != 0)
        (p :: acc._1, acc._2)
      else
        (List[String](), Passport(acc._1.flatMap(_.split(" "))) :: acc._2)
    }
    if (raw._1.nonEmpty)
      Passport(raw._1.flatMap(_.split(" "))) :: raw._2
    else
      raw._2
  }

  def countPassports(file: BufferedSource) = {
    readFile(file).count(_.isDefined)
  }
}
