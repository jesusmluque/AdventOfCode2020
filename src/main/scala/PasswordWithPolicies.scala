object PasswordWithPolicies {

  def checkPasswordWithPolicy(passAndPolicy: String)(policy:(String, String, String, String) => Boolean) = {
    val List(a:String, b:String ,pass:String) = passAndPolicy.split(" ").toList
    val List(min:String, max:String) = a.split("-").toList
    val letter = b.takeWhile(c => c != ':')

    policy(pass,min,max,letter)
  }

  def totalCount(passwords: List[String]) = {
    passwords.count(checkPasswordInterval)
  }

  def checkPasswordInterval(passAndPolicy: String) = {
    checkPasswordWithPolicy(passAndPolicy) { (pass,min,max,letter) =>
      val count = pass.count(_.toString == letter)
      count >= min.toInt && count <= max.toInt
    }
  }
  def checkPassword2(passAndPolicy: String) = {
    checkPasswordWithPolicy(passAndPolicy) { (pass,min,max,letter) =>
      (letter == pass(min.toInt - 1).toString || letter == pass(max.toInt - 1).toString) &&
        !(letter == pass(min.toInt -1).toString && letter == pass(max.toInt - 1).toString)
    }
  }

  def totalCount2(passwords: List[String]) = {
    passwords.count(checkPassword2)
  }
}
