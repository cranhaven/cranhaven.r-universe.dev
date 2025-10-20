#' FG_CC
#'
#' Case-control sample extracted from Fang and Gong (2020).
#' The case-control sample is extracted from Fang and Gong (2020) by the following procedure.
#' The case sample is composed of 2,261 flagged physicians (that is, those who billed for more than 100 hours per week).
#' The control sample of equal size is randomly drawn without replacement from the pool of physicians who were never flagged.
#' The sample is composed of 4,522 physicians who billed at least 20 hours per week.
#' @format A data frame with 4,522 rows and 5 variables:
#' \describe{
#'   \item{male}{indicator: physician is male}
#'   \item{isMD}{indicator: physician has a MD degree}
#'   \item{experYear}{experience in years}
#'   \item{flag}{indicator: physician billed for more than 100 hours per week}
#'   \item{smallPractice}{indicator: number of group practice members less than 6}
#' }
#' @source Fang, H. and Gong, Q. (2020) Data and Code for: Detecting Potential Overbilling in Medicare Reimbursement via Hours Worked: Reply.
#' Nashville, TN: American Economic Association [publisher]. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2020-11-23.
#' \doi{10.3886/E119192V1}
#' @references Fang, H. and Gong, Q. (2017). Detecting Potential Overbilling in Medicare Reimbursement via Hours Worked.
#' American Economic Review, 107(2), 562-91.
#' @references Matsumoto, B. (2020). Detecting Potential Overbilling in Medicare Reimbursement via Hours Worked: Comment.
#' American Economic Review, 110(12), 3991-4003.
#' @references Fang, H. and Gong, Q. (2020). Detecting Potential Overbilling in Medicare Reimbursement via Hours Worked: Reply.
#' American Economic Review, 110(12), 4004-10.
"FG_CC"
