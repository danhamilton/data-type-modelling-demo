import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration

class TaggedTypeSpec extends FlatSpec with Matchers  {

  import TaggedTypes._

  val url = "http:/www.test.com"
  val amount = 1000
  val interestRate = 0.1
  val apr = 0.15

  val urlS: UrlT = url.asUrl
  val loanAmountS: LoanAmountT = amount.asLoanAmount
  val duration: FiniteDuration = FiniteDuration(100, "days")
  val interestRateS = interestRate.asInterestRate
  val aprS = apr.asApr
  val cat: Option[PersonalLoan.type] = Option(PersonalLoan)

  "Tagged Type" should "create a Loan for valid values" in {

    val loan = TaggedTypeLoan(urlS, loanAmountS, duration, interestRateS, aprS, cat)

      loan shouldBe a[TaggedTypeLoan]
      loan.loanAmount shouldBe amount
      loan.apr shouldBe apr
      loan.interestRate shouldBe interestRate
  }

  it should "guard against misordered parameters" in {
    "val badLoan = TaggedTypeLoan(urlS, loanAmountS, duration, aprS, interestRateS, cat)" shouldNot compile
  }

  it should "guard against invalid url" in {

    val badLoan: TaggedTypeLoan = TaggedTypeLoan("badURL*^&(*&".asUrl, loanAmountS, duration, interestRateS, aprS, cat)
    badLoan match {
      case x: TaggedTypeLoan => fail(s"Should not be a loan. Found $x")
      case _                 => succeed
    }
  }

  it should "guard against invalid amount" in {
    val badLoan: TaggedTypeLoan = TaggedTypeLoan(urlS, -321.asLoanAmount, duration, interestRateS, aprS, cat)
    badLoan match {
      case x: TaggedTypeLoan => fail(s"Should not be a loan. Found $x")
      case _                 => succeed
    }
  }

  it should "guard against invalid interest rate" in {
    val badLoan: TaggedTypeLoan = TaggedTypeLoan(urlS, loanAmountS, duration, -3.asInterestRate, aprS, cat)
    badLoan match {
      case x: TaggedTypeLoan => fail(s"Should not be a loan. Found $x")
      case _                 => succeed
    }
  }

  it should "guard against invalid apr" in {
    val badLoan: TaggedTypeLoan = TaggedTypeLoan(urlS, loanAmountS, duration, interestRateS, -0.5.asApr, cat)
    badLoan match {
      case x: TaggedTypeLoan => fail(s"Should not be a loan. Found $x")
      case _                 => succeed
    }
  }

  it should "guard against invalid loan category" in {
    """val badLoan = TaggedTypeLoan(urlS, loanAmountS, duration, interestRateS, aprS, Option("aljd"))""" shouldNot compile
  }

  it should "guard against invalid copy" in {
    val goodLoan: TaggedTypeLoan = TaggedTypeLoan(urlS, loanAmountS, duration, interestRateS, aprS, cat)

    "goodLoan.copy(loanAmount = -321.asLoanAmount)" shouldNot compile
  }
}

