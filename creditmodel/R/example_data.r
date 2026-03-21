#' UCI Credit Card data
#'
#'  This research aimed at the case of customers's default payments in Taiwan and compares the predictive accuracy of probability of default among six data mining methods.
#'  This research employed a binary variable, default payment (Yes = 1, No = 0), as the response variable. This study reviewed the literature and used the following 24 variables as explanatory variables
#'
#' \itemize{
#'   \item ID: Customer id
#'   \item apply_date: This is a fake occur time.
#'   \item LIMIT_BAL: Amount of the given credit (NT dollar): it includes both the individual consumer credit and his/her family (supplementary) credit.
#'   \item SEX:  Gender (male; female).
#'   \item EDUCATION: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
#'   \item MARRIAGE: Marital status (1 = married; 2 = single; 3 = others).
#'   \item AGE: Age (year)
#' History of past payment. We tracked the past monthly payment records (from April to September, 2005) as follows:
#'   \item PAY_0: the repayment status in September
#'   \item PAY_2: the repayment status in August
#'   \item PAY_3: ...
#'   \item PAY_4: ...
#'   \item PAY_5: ...
#'   \item PAY_6: the repayment status in April
#' The measurement scale for the repayment status is: -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months;...;8 = payment delay for eight months; 9 = payment delay for nine months and above.
#' Amount of bill statement (NT dollar)
#'   \item BILL_AMT1: amount of bill statement in September
#'   \item BILL_AMT2: mount of bill statement in August
#'   \item BILL_AMT3: ...
#'   \item BILL_AMT4: ...
#'   \item BILL_AMT5: ...
#'   \item BILL_AMT6: amount of bill statement in April
#'  Amount of previous payment (NT dollar)
#'   \item PAY_AMT1: amount paid in September
#'   \item PAY_AMT2: amount paid in August
#'   \item PAY_AMT3: ....
#'   \item PAY_AMT4: ...
#'   \item PAY_AMT5: ...
#'   \item PAY_AMT6: amount paid in April
#'   \item default.payment.next.month: default payment (Yes = 1, No = 0), as the response variable
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 30000 rows and 26 variables.
#' @name UCICreditCard
#' @source \url{http://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients}
#' @seealso \code{\link{lendingclub}}
NULL

#' Lending Club data
#'
#'  This data contains complete loan data for all loans issued through the time period stated, including the current loan status (Current, Late, Fully Paid, etc.) and latest payment information.
#'  The data containing loan data through the "present" contains complete loan data for all loans issued through the previous completed calendar quarter(time period: 2018Q1:2018Q4).
#'
#' \itemize{
#'   \item id: A unique LC assigned ID for the loan listing.
#'   \item issue_d: The month which the loan was funded.
#'   \item loan_status: Current status of the loan.
#'   \item addr_state: The state provided by the borrower in the loan application.
#'   \item acc_open_past_24mths: Number of trades opened in past 24 months.
#'   \item all_util: Balance to credit limit on all trades.
#'   \item annual_inc: The self:reported annual income provided by the borrower during registration.
#'   \item avg_cur_bal: Average current balance of all accounts.
#'   \item bc_open_to_buy: Total open to buy on revolving bankcards.
#'   \item bc_util: Ratio of total current balance to high credit/credit limit for all bankcard accounts.
#'   \item dti: A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower's self:reported monthly income.
#'   \item dti_joint: A ratio calculated using the co:borrowers' total monthly payments on the total debt obligations, excluding mortgages and the requested LC loan, divided by the co:borrowers' combined self:reported monthly income
#'   \item emp_length: Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.
#'   \item emp_title: The job title supplied by the Borrower when applying for the loan.
#'   \item funded_amnt_inv: The total amount committed by investors for that loan at that point in time.
#'   \item grade: LC assigned loan grade
#'   \item inq_last_12m: Number of credit inquiries in past 12 months
#'   \item installment: The monthly payment owed by the borrower if the loan originates.
#'   \item max_bal_bc: Maximum current balance owed on all revolving accounts
#'   \item mo_sin_old_il_acct: Months since oldest bank installment account opened
#'   \item mo_sin_old_rev_tl_op: Months since oldest revolving account opened
#'   \item mo_sin_rcnt_rev_tl_op: Months since most recent revolving account opened
#'   \item mo_sin_rcnt_tl: Months since most recent account opened
#'   \item mort_acc: Number of mortgage accounts.
#'   \item pct_tl_nvr_dlq: Percent of trades never delinquent
#'   \item percent_bc_gt_75: Percentage of all bankcard accounts > 75% of limit.
#'   \item purpose: A category provided by the borrower for the loan request.
#'   \item sub_grade: LC assigned loan subgrade
#'   \item term: The number of payments on the loan. Values are in months and can be either 36 or 60.
#'   \item tot_cur_bal: Total current balance of all accounts
#'   \item tot_hi_cred_lim: Total high credit/credit limit
#'   \item total_acc: The total number of credit lines currently in the borrower's credit file
#'   \item total_bal_ex_mort: Total credit balance excluding mortgage
#'   \item total_bc_limit: Total bankcard high credit/credit limit
#'   \item total_cu_tl: Number of finance trades
#'   \item total_il_high_credit_limit: Total installment high credit/credit limit
#'   \item verification_status_joint: Indicates if the co:borrowers' joint income was verified by LC, not verified, or if the income source was verified
#'   \item zip_code: The first 3 numbers of the zip code provided by the borrower in the loan application.
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 63532 rows and 145 variables.
#' @name lendingclub
#' @seealso \code{\link{UCICreditCard}}
NULL

#' Entropy Weight Method Data
#'
#'  This data is for Entropy Weight Method examples.
#' @docType data
#' @keywords datasets
#' @format A data frame with 10 rows and 13 variables.
#' @name ewm_data
NULL

