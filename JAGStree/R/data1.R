#' Simple Tree Data 1
#'
#' Small, artificially generated toy data set to demonstrate package functionality
#'
#' @docType data
#'
#' @usage data(data1)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{from}{A node label and started point of directed edge (parent node)}
#'  \item{to}{A node label and endpoint of directed edge (child node)}
#'  \item{Estimate}{A numerical value assumed to be survey count belonging to 'to' node (integer)}
#'  \item{Total}{A numerical value assumed to be survey sample size (integer)}
#'  \item{Count}{A numerical value for marginal count if leaf node (integer)}
#'  \item{Population}{A boolean value for if survey size is entire population (logical)}
#'  \item{Description}{A string describing 'to' node (string)}
#' }
#' @references This data set was artificially created for the JAGStree package.
#' @keywords datasets
#' @examples
#' data(data1)
#' head(data1)
#'
"data1"
