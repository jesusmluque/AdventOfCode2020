import org.scalatest.FlatSpec

import scala.io.Source

class PasswordWithPoliciesTest extends FlatSpec  {

  "Check password: 1-3 a: abcde, 1-3 b: cdefg, 2-9 c: ccccccccc " should "be 2" in {
    assert(PasswordWithPolicies.totalCount(List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")) === 2)
  }

  "Check passwords from file " should "be 2" in {
    assert(PasswordWithPolicies.totalCount(Source.fromResource("passwords.txt").getLines().toList) === 625)
  }

  "Check password2: 1-3 a: abcde, 1-3 b: cdefg, 2-9 c: ccccccccc " should "be 1" in {
    assert(PasswordWithPolicies.totalCount2(List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")) === 1)
  }

  "Check passwords2 from file " should "be 2" in {
    assert(PasswordWithPolicies.totalCount2(Source.fromResource("passwords.txt").getLines().toList) === 391)
  }
}

