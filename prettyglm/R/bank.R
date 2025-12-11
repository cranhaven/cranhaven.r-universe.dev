#' Bank marketing campaigns data set analysis
#'
#' It is a dataset that describing Portugal bank marketing campaigns results.
#' Conducted campaigns were based mostly on direct phone calls, offering bank client to place a term deposit.
#' If after all marking efforts client had agreed to place deposit - target variable marked 'yes', otherwise 'no'
#'
#' Sourse of the data
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing
#'
#' @docType data
#'
#' @usage data(bank)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{job}{Type of job}
#'  \item{marital}{marital status}
#'  \item{education}{education}
#'  \item{default}{has credit in default?}
#'  \item{housing}{has housing loan?}
#'  \item{loan}{has personal loan?}
#'  \item{age}{age}
#'  \item{y}{has the client subscribed a term deposit? (binary: "yes","no")}
#' }
#' @references This dataset is public available for research. The details are described in S. Moro, P. Cortez and P. Rita. A Data-Driven Approach to Predict the Success of Bank Telemarketing. Decision Support Systems, Elsevier, 62:22-31, June 2014
#' @keywords datasets
#' @examples
#'
#' data(bank)
#' head(bank_data)
'bank_data'
