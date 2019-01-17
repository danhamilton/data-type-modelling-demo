import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex
//////// Value class with Smart Constructors ///////////

case class UrlSmart private(value: String) extends AnyVal {
  private def copy() = ()
}
object UrlSmart {
  def apply(value: String): Option[UrlSmart] = {
    val regex: Regex = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
    value match {
      case regex(_*) => Option(new UrlSmart(value))
      case _         => None
    }
  }
}

case class LoanAmountSmart private (value: Int) extends AnyVal {
  private def copy() = ()
}
object LoanAmountSmart {
  def apply(value: Int): Option[LoanAmountSmart] = {
    if(value > 0) Option(new LoanAmountSmart(value))
    else None
  }
}

case class InterestRateSmart private (value: Double) extends AnyVal {
  private def copy() = ()
}
object InterestRateSmart {
  def apply(value: Double): Option[InterestRateSmart] = {
    if(value > 0) Option(new InterestRateSmart(value))
    else None
  }
}

case class AprSmart private (value: Double) extends AnyVal {
  private def copy() = ()
}
object AprSmart {
  def apply(value: Double): Option[AprSmart] = {
    if(value > 0) Option(new AprSmart(value))
    else None
  }
}


case class LoanValueClassSmart(url: UrlSmart,
                               loanAmount: LoanAmountSmart,
                               duration: FiniteDuration,
                               interestRate: InterestRateSmart,
                               apr: AprSmart,
                               loanCategory: Option[LoanCat] = Option(PersonalLoan))

object LoanValueClassSmart {

  // Danger - vulnerable to parameter misordering (interestRate and Apr).
  // Consider deleting this apply method if you have multiple parameters of same primitive
  def apply(url: String, loanAmount: Int, duration: FiniteDuration, interestRate: Double, apr: Double): Option[LoanValueClassSmart] =
    for {
      urlV    <- UrlSmart(url)
      amountV <- LoanAmountSmart(loanAmount)
      intV    <- InterestRateSmart(interestRate)
      aprV    <- AprSmart(apr)
    } yield
      new LoanValueClassSmart(urlV, amountV, duration, intV, aprV)
}



