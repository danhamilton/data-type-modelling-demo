import TaggedTypesValidated.{AprTV, InterestRateTV, LoanAmountTV, UrlTV}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex
//////// Tagged Type with validation ///////////

object TaggedTypesValidated {

  import shapeless.tag
  import shapeless.tag.@@

  trait UrlTagV
  type UrlTV = String @@ UrlTagV

  trait LoanAmountTagV
  type LoanAmountTV = Int @@ LoanAmountTagV

  trait InterestRateTagV
  type InterestRateTV = Double @@ InterestRateTagV

  trait AprTagV
  type AprTV = Double @@ AprTagV

  implicit class StringTagOpsV(value: String) {
    def asUrlV(): Option[UrlTV] = {
      val regex: Regex = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
      value match {
        case regex(_*) => Option(tag[UrlTagV][String](value))
        case _         => None
      }
    }
  }

  implicit class IntTagOpsV(value: Int) {
    def asLoanAmountV(): Option[LoanAmountTV]  =
      if(value > 0 ) Option(tag[LoanAmountTagV][Int](value))
      else None
  }

  implicit class DoubleTagOpsV(value: Double) {
    def asAprV(): Option[AprTV]                   = {
      if(value > 0) Option(tag[AprTagV][Double](value))
      else None
    }
    def asInterestRateV(): Option[InterestRateTV] = {
      if(value > 0 ) Option(tag[InterestRateTagV][Double](value))
      else None
    }
  }
}


case class TaggedTypeValidatedLoan(uRL: UrlTV,
                                   loanAmount: LoanAmountTV,
                                   duration: FiniteDuration,
                                   interestRate: InterestRateTV,
                                   apr: AprTV,
                                   loanCategory: Option[LoanCat] = Option(PersonalLoan))

