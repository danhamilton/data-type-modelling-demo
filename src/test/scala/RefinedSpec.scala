import java.io

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.string.Url
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration

class RefinedSpec extends FlatSpec with Matchers  {

  val url = "http:/www.test.com"
  val amount = 1000
  val interestRate = 0.1
  val apr = 0.15

  val urlS: Either[String, Refined[String, Url]] = refineV[Url](url)
  val loanAmountS = LoanAmountSmart(amount).get
  val duration: FiniteDuration = FiniteDuration(100, "days")
  val interestRateS = InterestRateSmart(interestRate).get
  val aprS = AprSmart(apr).get
  val cat: Option[PersonalLoan.type] = Option(PersonalLoan)

  "Refined" should "create a Loan for valid values" in {

    // Safest way to construct
    val goodLoan: Either[String, RefinedLoan] = for {
      u <- refineV[Url](url)
      a <- refineV[Positive](amount)
      i <- refineV[Positive](interestRate)
      p <- refineV[Positive](apr)
    } yield new RefinedLoan(u, a, duration, i, p, cat)

    goodLoan.isRight shouldBe true

    goodLoan.map(loan => {
      loan shouldBe a[RefinedLoan]
      loan.url.value shouldBe url
      loan.loanAmount.value shouldBe amount
      loan.apr.value shouldBe apr
      loan.interestRate.value shouldBe interestRate
    })
  }

  it should "guard against misordered parameters" in {
    "val badLoan: Either[String, RefinedLoan] = RefinedLoan(url, amount, duration, apr, interestRate, cat)" shouldNot compile
  }

  it should "guard against invalid url" in {

    val badLoan: Either[String, RefinedLoan] = RefinedLoan("badURL*^&(*&", amount, duration, interestRate, apr, cat)
    badLoan match {
      case Right(x: RefinedLoan) => fail(s"Should not be a loan. Found $x")
      case _                     => succeed
    }
  }

  it should "guard against invalid amount" in {
    val badLoan: Either[String, RefinedLoan] = RefinedLoan(url, -5000, duration, interestRate, apr, cat)
    badLoan match {
      case Right(x: RefinedLoan) => fail(s"Should not be a loan. Found $x")
      case _                     => succeed
    }
  }

  it should "guard against invalid interest rate" in {
    val badLoan: Either[String, RefinedLoan] = RefinedLoan(url, amount, duration, -2, apr, cat)
    badLoan match {
      case Right(x: RefinedLoan) => fail(s"Should not be a loan. Found $x")
      case _                     => succeed
    }
  }

  it should "guard against invalid apr" in {
    val badLoan: Either[String, RefinedLoan] = RefinedLoan(url, amount, duration, interestRate, -3, cat)
    badLoan match {
      case Right(x: RefinedLoan) => fail(s"Should not be a loan. Found $x")
      case _                     => succeed
    }
  }

  it should "guard against invalid loan category" in {
    """val badLoan = RefinedLoan(url, amount, duration, interestRate, apr, Option("aljd"))""" shouldNot compile
  }

  it should "guard against invalid copy" in {
    val goodLoan: Either[String, RefinedLoan] = RefinedLoan(url, amount-100, duration, interestRate, apr, cat)

    goodLoan.isRight shouldBe true

    val runTimeUrl: String = "bad&^*&^*&"

    goodLoan map { loan =>
      "loan.copy(url = runTimeUrl)" shouldNot compile
    }
  }
}

