import scala.concurrent.duration.FiniteDuration


//////// Value Class Implementation //////////

final case class UrlValue(value: String) extends AnyVal
final case class LoanAmount (value: Int) extends AnyVal
final case class InterestRate(value: Double) extends AnyVal
final case class Apr(value: Double) extends AnyVal

trait LoanCat {
  def name: String
}

object PersonalLoan extends LoanCat {
  override def name: String = "Personal_Loan"
}

object HirePurchaseLoan extends LoanCat {
  override def name: String = "HirePurchase"
}

case class LoanValueClass(url: UrlValue,
                          loanAmount: LoanAmount,
                          duration: FiniteDuration,
                          interestRate: InterestRate,
                          apr: Apr,
                          loanCategory: Option[LoanCat] = Option(PersonalLoan))

