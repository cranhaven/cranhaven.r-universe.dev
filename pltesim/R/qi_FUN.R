#' Function for finding predicted probability from the systematic linear
#' component of a logistic regression.
#'
#' @param x a numeric vector of the systematic linear component from a
#' logistic regression model.
#'
#' @export

logistic_prob_FUN <- function(x) {
    1 / (1 + exp(-x))
}
