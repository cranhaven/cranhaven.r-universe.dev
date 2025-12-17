#' Simulated Data with Environment Dependent Correlations
#'
#' A dataset containing simulated data for example use of the \code{eclust}
#' package functions. This data was generated using the \code{\link{s_modules}}
#' and \code{\link{s_generate_data}}
#'
#' @format A matrix with 100 rows and 502 variables: \describe{
#'   \item{Y}{continuous response vector} \item{E}{binary environment variable
#'   for ECLUST method. E = 0 for unexposed (n=50) and E = 1 for exposed (n=50)} \item{columns
#'   3:502}{gene expression data for 1000 genes. column names are the gene
#'   names } }
#' @note Code used to generate this data can be found on the GitHub page for
#'   this package. See URL below.
#' @source
#'   \url{https://raw.githubusercontent.com/sahirbhatnagar/eclust/master/data-raw/simulated-data-processing.R}
#'
#'
#' @references Bhatnagar, SR., Yang, Y., Blanchette, M., Bouchard, L.,
#'   Khundrakpam, B., Evans, A., Greenwood, CMT. (2016+). \emph{An analytic
#'   approach for interpretable predictive models in high dimensional data, in
#'   the presence of interactions with exposures
#'   \href{http://sahirbhatnagar.com/slides/manuscript1_SB_v4.pdf}{Preprint}}
#' @examples
#' simdata[1:5, 1:10]
#' table(simdata[,"E"])
"simdata"

