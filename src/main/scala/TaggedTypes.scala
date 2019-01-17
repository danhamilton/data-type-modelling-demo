import TaggedTypes.{AprT, InterestRateT, LoanAmountT, UrlT}

import scala.concurrent.duration.FiniteDuration


object TaggedTypes {

  import shapeless.tag
  import shapeless.tag.@@

  trait UrlTag
  type UrlT = String @@ UrlTag

  trait LoanAmountTag
  type LoanAmountT = Int @@ LoanAmountTag

  trait InterestRateTag
  type InterestRateT = Double @@ InterestRateTag

  trait AprTag
  type AprT = Double @@ AprTag

  implicit class StringTagOps(value: String) {
    def asUrl(): UrlT = tag[UrlTag][String](value)
  }

  implicit class BigDecimalTagOps(value: Int) {
    def asLoanAmount(): LoanAmountT  = tag[LoanAmountTag][Int](value)
  }

  implicit class DoubleTagOps(value: Double) {
    def asApr(): AprT                   = tag[AprTag][Double](value)
    def asInterestRate(): InterestRateT = tag[InterestRateTag][Double](value)
  }
}

case class TaggedTypeLoan(url: UrlT,
                          loanAmount: LoanAmountT,
                          duration: FiniteDuration,
                          interestRate: InterestRateT,
                          apr: AprT,
                          loanCategory: Option[LoanCat] = Option(PersonalLoan))
