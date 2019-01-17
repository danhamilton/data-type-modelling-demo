import org.scalatest.{FlatSpec, Matchers}

class LoanSpec extends FlatSpec with Matchers  {

  val url = "test.com"
  val loanAmount = 1000
  val duration = 10
  val interestRate = 0.1
  val apr = 0.15
  val cat = Option(LoanCategory.PERSONAL)

  "Loan Class" should "create a Loan for valid values" in {

    val loan = Loan(url, loanAmount, duration, interestRate, apr, cat)

    loan shouldBe a[Loan]
    loan.loanAmount shouldBe loanAmount
    loan.apr shouldBe apr
    loan.interestRate shouldBe interestRate
  }

  it should "guard against misordered parameters" in {
    val badLoan = Loan(url, loanAmount, duration, apr, interestRate, cat)
    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid url" in {
    val badLoan = Loan("bad&*&^*&^*", loanAmount, duration, interestRate, apr, cat)
    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid amount" in {
    val badLoan = Loan(url, -5000, duration, interestRate, apr, cat)
    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid interest rate" in {
    val badLoan = Loan(url, loanAmount, duration, -2, apr, cat)
    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid apr" in {
    val badLoan = Loan(url, loanAmount, duration, interestRate, -3, cat)
    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid loan category" in {
    val badLoan = Loan(url, loanAmount, duration, interestRate, apr, Option("some crap"))
    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }

  it should "guard against invalid copy" in {
    val goodLoan = Loan(url, loanAmount, duration, interestRate, apr, cat)
    val badLoan = goodLoan.copy(loanAmount = -5000)

    badLoan match {
      case x: Loan => fail(s"Should not be a loan. Found $x")
      case _       => succeed
    }
  }
}

