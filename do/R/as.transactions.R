#' Transform to transactions
#'
#' @param x dataframe or matrix
#' @return a transaction data
#' @export
#'
as.transactions <- function(x){
    as(as.matrix(x), "transactions")
}
