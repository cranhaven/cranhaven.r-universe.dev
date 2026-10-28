#' Gini correlation index
#'
#' This function calculates the Gini correlation between two distributions.
#' @param x a numeric vector containing at least non-negative elements.
#' @param y a numeric vector containing the distribution with the rank information.
#' @param weights an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @returns The value of the Gini correlation, should be between -1 and 1.
#' @export
#' @examples
#'
#' # Calculate the gini correlation between the salary and total income distributions
#'
#'
#' Salary_distribution = sample_income_data$wage
#' Total_income_distribution = rowSums(sample_income_data[3:6])
#'
#' gini_corr(Salary_distribution, Total_income_distribution)
#'
#'
#' @references
#'
#' E. Schechtman and S. Yitzhaki (1999) \emph{On the proper bounds of the Gini correlation},
#' Economics Letters,Volume 63, Issue 2, p. 133-138, ISSN 0165-1765
#'
#' Handcock, M. (2016), \emph{Relative Distribution Methods in the Social Sciences}, Springer-Verlag, Inc., New York, 1999 ISBN 0387987789
#'
#'


gini_corr <- function(x, y, weights = NULL) {
  gini_wtd_ord(x, y, weights) / gini_wtd_ord(x, weights = weights)
}
