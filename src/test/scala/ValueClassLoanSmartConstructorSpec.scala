import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration

class ValueClassLoanSmartConstructorSpec extends FlatSpec with Matchers  {

  val url = "http:/www.test.com"
  val amount = 1000
  val interestRate = 0.1
  val apr = 0.15

  val urlS = UrlSmart(url).get
  val loanAmountS = LoanAmountSmart(amount).get
  val duration: FiniteDuration = FiniteDuration(100, "days")
  val interestRateS = InterestRateSmart(interestRate).get
  val aprS = AprSmart(apr).get
  val cat: Option[PersonalLoan.type] = Option(PersonalLoan)

  "Loan Value Smart Constructor Class" should "create a Loan for valid values" in {

    // Safest way to construct
    val goodLoan: Option[LoanValueClassSmart] = for {
      u <- UrlSmart(url)
      a <- LoanAmountSmart(amount)
      i <- InterestRateSmart(interestRate)
      p <- AprSmart(apr)
    } yield LoanValueClassSmart(u, a, duration, i, p, cat)


    goodLoan.fold(fail("Should be a LoanValueClassSmart"))(loan => {

      loan shouldBe a[LoanValueClassSmart]
      loan.loanAmount shouldBe loanAmountS
      loan.apr shouldBe aprS
      loan.interestRate shouldBe interestRateS
    }
    )
  }

  it should "guard against misordered parameters" in {
    "val badLoan = LoanValueClassSmart(url, loanAmount, duration, apr, interestRate, cat)" shouldNot compile
  }

  it should "guard against invalid url" in {

    val badLoan: Option[LoanValueClassSmart] = LoanValueClassSmart("badURL*^&(*&", amount, duration, interestRate, apr)
    badLoan match {
      case Some(x: LoanValueClassSmart) => fail(s"Should not be a loan. Found $x")
      case _                            => succeed
    }
  }

  it should "guard against invalid amount" in {
    val badLoan: Option[LoanValueClassSmart] = LoanValueClassSmart(url, -5000, duration, interestRate, apr)
    badLoan match {
      case Some(x: LoanValueClassSmart) => fail(s"Should not be a loan. Found $x")
      case _                            => succeed
    }
  }

  it should "guard against invalid interest rate" in {
    val badLoan: Option[LoanValueClassSmart] = LoanValueClassSmart(url, amount, duration, -2, apr)
    badLoan match {
      case Some(x: LoanValueClassSmart) => fail(s"Should not be a loan. Found $x")
      case _                            => succeed
    }
  }

  it should "guard against invalid apr" in {
    val badLoan: Option[LoanValueClassSmart] = LoanValueClassSmart(url, amount, duration, interestRate, -3)
    badLoan match {
      case Some(x: LoanValueClassSmart) => fail(s"Should not be a loan. Found $x")
      case _                            => succeed
    }
  }

  it should "guard against invalid loan category" in {
    """val badLoan = LoanValueClassSmart(url, loanAmount, duration, interestRate, apr, Option("aljd"))""" shouldNot compile

  }

  it should "guard against invalid copy" in {
    val goodLoan: Option[LoanValueClassSmart] = LoanValueClassSmart(url, amount-100, duration, interestRate, apr)

    """goodLoan.get.copy(loanAmount = loanAmountS.copy(-5000))""" shouldNot compile
  }
}

