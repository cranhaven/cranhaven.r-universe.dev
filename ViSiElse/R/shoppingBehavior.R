#' Simulated online shopping behavior time data
#'
#' This dataset shows the buying process of consumers over internet based on a 
#' 5-steps model: need recognition, information search, evaluation, purchase 
#' decision, and post-purchase behavior. This simulated dataset of 100 subjects
#' correspond to the timestamps (in s) of each action of the model (except for
#' the post-purchase behavior) executed by the subjects.
#'
#' @docType data
#'
#' @usage data(shoppingBehavior)
#'
#' @format A data frame with 100 rows and 7 variables:
#' \describe{
#'   \item{id}{Customer ID.}
#'   \item{need}{Time (in s) when the customer decides he/she needs an item.}
#'   \item{start_search}{Time (in s) when the customer starts to search for the item.}
#'   \item{stop_search}{Time (in s) when the customer stops to search for the item.}
#'   \item{start_eval}{Time (in s) when the customer starts to evaluate the item.}
#'   \item{stop_eval}{Time (in s) when the customer stops to evaluate the item.}
#'   \item{deci}{Time (in s) when the customer decides to buy the item.}
#' }
#'
#' @keywords datasets
#'
#' @references Garnier EM, Fouret N, Descoins M (2019) ViSiElse: An innovative 
#' R-package to visualize raw behavioral data over time.
#'  PeerJ Preprints 10.7287/peerj.preprints.27665v2
#' ([PeerJ](https://doi.org/10.7287/peerj.preprints.27665v2))
#'
#' @examples
#' data(shoppingBehavior)
#' head(shoppingBehavior)
"shoppingBehavior"
