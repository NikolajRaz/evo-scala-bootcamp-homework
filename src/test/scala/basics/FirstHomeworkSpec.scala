package basics

import FirstHomework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FirstHomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "lcm" should "be correct" in {
    lcm(2, 3) shouldEqual 6
    lcm(12, -24) shouldEqual 24
    lcm(-4, 5) shouldEqual 20
    lcm(12, 18) shouldEqual 36
    lcm(0, 0) shouldEqual 0
    lcm(0, 1) shouldEqual 0
    lcm(1, 0) shouldEqual 0
  }

  "gcd" should "be correct" in {
    gcd(54, 24) shouldEqual 6
    gcd(8, 80) shouldEqual 8
    gcd(18, -27) shouldEqual 9
    gcd(-30, 50) shouldEqual 10
    gcd(0, 0) shouldEqual 0
    gcd(0, 1) shouldEqual 0
    gcd(1, 0) shouldEqual 0
  }
}

