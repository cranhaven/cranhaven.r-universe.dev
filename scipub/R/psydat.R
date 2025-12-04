#' Sample demographic and clinical data for 5,000 children
#'
#' An example dataset containing demographic and clinical
#' data for 5,000 children. The variables are as follows:
#'
#' @docType data
#' @name psydat
#' @aliases psydat
#' @usage data(psydat)
#'
#' @format A data frame with 5000 rows and 7 variables:
#' \describe{
#'   \item{Age}{age in months (107.2--136.4)}
#'   \item{Sex}{biological sex, 4 missing value (M, F)}
#'   \item{Income}{reported family income,
#'    404 missing values (<50K, >=100K, >=50K&<100K)}
#'   \item{Height}{height in inches, 7 missing values (36.05--84.51)}
#'   \item{iq}{cognition test, 179 missing values (34.86--222.99)}
#'   \item{depressT}{depression symptom severity T-score,
#'    8 missing values (48.53--91.32)}
#'   \item{anxT}{anxiety symptom severity T-score,
#'    8 missing values (48.76--93,67)}
#' }
"psydat"
