import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration

class ValueClassLoanSpec extends FlatSpec with Matchers  {

  val url = UrlValue("test.com")
  val loanAmount = LoanAmount(1000)
  val duration = FiniteDuration(100, "days")
  val interestRate = InterestRate(0.1)
  val apr = Apr(0.15)
  val cat = Option(PersonalLoan)

  "Loan Value Class" should "create a Loan for valid values" in {

    val loan = LoanValueClass(url, loanAmount, duration, interestRate, apr, cat)

    loan shouldBe a[LoanValueClass]
    loan.loanAmount shouldBe loanAmount
    loan.apr shouldBe apr
    loan.interestRate shouldBe interestRate
  }

  it should "guard against misordered parameters" in {
    "val badLoan = LoanValueClass(url, loanAmount, duration, apr, interestRate, cat)" shouldNot compile
  }

  it should "guard against invalid url" in {
    val badLoan = LoanValueClass(UrlValue("bad&*&^*&^*"), loanAmount, duration, interestRate, apr, cat)
    badLoan match {
      case x: LoanValueClass => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid amount" in {
    val badLoan = LoanValueClass(url, LoanAmount(-5000), duration, interestRate, apr, cat)
    badLoan match {
      case x: LoanValueClass => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid interest rate" in {
    val badLoan = LoanValueClass(url, loanAmount, duration, InterestRate(-2), apr, cat)
    badLoan match {
      case x: LoanValueClass => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid apr" in {
    val badLoan = LoanValueClass(url, loanAmount, duration, interestRate, Apr(-3), cat)
    badLoan match {
      case x: LoanValueClass => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid loan category" in {
    """val badLoan = LoanValueClass(url, loanAmount, duration, interestRate, apr, Option("aljd"))""" shouldNot compile

  }

  it should "guard against invalid copy" in {
    val goodLoan = LoanValueClass(url, loanAmount, duration, interestRate, apr, cat)
    val badLoan = goodLoan.copy(loanAmount = LoanAmount(-5000))

    badLoan match {
      case x: LoanValueClass => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }
}

