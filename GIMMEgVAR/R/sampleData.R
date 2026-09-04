#' sampleData
#'
#' List containing 10 elements.  Each element consists of a data frame with time series data that was
#' generated via simulation.
#'
#' @name sampleData
#' @docType data
#' @author Sandra AW Lee \email{wsandra@live.unc.edu}
#' @format ## `sampleData`
#' Each element in the list contains a data frame for an individual subject.  Each data frame has 150 observations
#' of 10 variables (V1-V10) that can be used for fitting GIMMEgVAR.  The variable Idnum is the unique subject ID.
#' \describe{
#'   \item{V1-V10}{Ten variables availalbe for us in fitting GIMMEgVAR}
#'   \item{IDnum}{Unique subject ID}
#' }
NULL
