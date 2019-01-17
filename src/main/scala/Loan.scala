
import LoanCategory.LoanCategory


///////// Basic Implementation /////////

object LoanCategory {
  type LoanCategory = String
  val PERSONAL: LoanCategory = "loans"
  val HIRE_PURCHASE: LoanCategory = "HirePurchase"
}

final case class Loan(url: String,
                      loanAmount: Int,
                      duration: Int,
                      interestRate: Double,
                      apr: Double,
                      loanCategory: Option[LoanCategory] = Some(LoanCategory.PERSONAL))
