import TaggedTypesValidated.{LoanAmountTV, UrlTV}
import org.scalatest.{FlatSpec, Matchers}
import shapeless.tag
import shapeless.tag.@@

import scala.concurrent.duration.FiniteDuration

class TaggedTypeValidatedSpec extends FlatSpec with Matchers  {

  import TaggedTypesValidated._

  val url = "http:/www.test.com"
  val amount = 1000
  val interestRate = 0.1
  val apr = 0.15

  val urlS: UrlTV = url.asUrlV.get
  val loanAmountS: LoanAmountTV = amount.asLoanAmountV.get
  val duration: FiniteDuration = FiniteDuration(100, "days")
  val interestRateS: InterestRateTV = interestRate.asInterestRateV.get
  val aprS: AprTV = apr.asAprV.get
  val cat: Option[PersonalLoan.type] = Option(PersonalLoan)


  def makeLoan(url: String = url, amount:Int = amount, interestRate: Double = interestRate, apr: Double = apr): Option[TaggedTypeValidatedLoan] =
    for {
      u <- url.asUrlV
      a <- amount.asLoanAmountV
      i <- interestRate.asInterestRateV
      p <- apr.asAprV
  } yield
    TaggedTypeValidatedLoan(u, a, duration, i, p, cat)


  "Tagged Type Validated" should "create a Loan for valid values" in {
    val goodLoan: Option[TaggedTypeValidatedLoan] = makeLoan()

      goodLoan.fold(fail("Should be a valid loan"))(loan => {

        loan shouldBe a[TaggedTypeValidatedLoan]
        loan.loanAmount shouldBe amount
        loan.apr shouldBe apr
        loan.interestRate shouldBe interestRate
      })
  }

  it should "guard against misordered parameters" in {
    "val badLoan = TaggedTypeValidatedLoan(urlS, loanAmountS, duration, aprS, interestRateS, cat)" shouldNot compile
  }

  it should "guard against invalid url" in {

    val badLoan: Option[TaggedTypeValidatedLoan] = makeLoan(url = "badUrlY*&^")
    badLoan match {
      case Some(x: TaggedTypeValidatedLoan) => fail(s"Should not be a loan. Found $x")
      case _                 => succeed
    }
  }

  it should "guard against invalid amount" in {
    val badLoan: Option[TaggedTypeValidatedLoan] = makeLoan(amount = -321)
    badLoan match {
      case Some(x: TaggedTypeValidatedLoan) => fail(s"Should not be a loan. Found $x")
      case _                                => succeed
    }
  }

  it should "guard against invalid interest rate" in {
    val badLoan: Option[TaggedTypeValidatedLoan] = makeLoan(interestRate = -0.34)
    badLoan match {
      case Some(x: TaggedTypeValidatedLoan) => fail(s"Should not be a loan. Found $x")
      case _                                => succeed
    }
  }

  it should "guard against invalid apr" in {
    val badLoan: Option[TaggedTypeValidatedLoan] = makeLoan(apr = 0)
    badLoan match {
      case Some(x: TaggedTypeValidatedLoan) => fail(s"Should not be a loan. Found $x")
      case _                                => succeed
    }
  }

  it should "guard against invalid loan category" in {
    """val badLoan = TaggedTypeValidatedLoan(urlS, loanAmountS, duration, interestRateS, aprS, Option("aljd"))""" shouldNot compile
  }

  it should "guard against invalid copy" in {
    val goodLoan: TaggedTypeValidatedLoan = TaggedTypeValidatedLoan(urlS, loanAmountS, duration, interestRateS, aprS, cat)

    "goodLoan.copy(loanAmount = -321.asLoanAmount)" shouldNot compile
  }

  it should "guard against renegade tagging with invalid values" in {

    val badTagAmount: LoanAmountTV = tag[LoanAmountTagV][Int](-1000)

    val badLoan = TaggedTypeValidatedLoan(urlS, badTagAmount, duration, interestRateS, aprS, cat)

    badLoan match {
      case x: TaggedTypeValidatedLoan => fail(s"Should not be a loan. Found $x")
      case _                          => succeed
    }
  }
}

