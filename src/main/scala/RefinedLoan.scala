import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.Url
import eu.timepit.refined._

import scala.concurrent.duration.FiniteDuration

////////// refined /////////
case class RefinedLoan(url: String Refined Url,
                       loanAmount: Int Refined Positive,
                       duration: FiniteDuration,
                       interestRate: Double Refined Positive,
                       apr: Double Refined Positive,
                       loanCategory: Option[LoanCat] = Option(PersonalLoan))


  object RefinedLoan {
    def apply(redirectUrl: String,
              loanAmount: Int,
              duration: FiniteDuration,
              interestRate: Double,
              apr: Double,
              loanCategory: Option[LoanCat]): Either[String, RefinedLoan] =
        for {
          urlV        <- refineV[Url](redirectUrl)
          loanAmountV <- refineV[Positive](loanAmount)
          intRateV    <- refineV[Positive](interestRate)
          aprV        <- refineV[Positive](apr)
        } yield
          new RefinedLoan(urlV, loanAmountV, duration, intRateV, aprV, loanCategory)
  }