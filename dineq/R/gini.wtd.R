#' @title Gini coefficient
#'
#' @description Returns the (optional weighted) Gini coefficient for a vector.
#'
#' @details The Gini coefficient is a measure of inequality among values of a distribution. The most
#' used single measure for income inequality. The coefficient can theoretically range between 0 and
#' 1, with 1 being the highest possible inequality (for instance: 1 person in a society has all
#' income; the others none). But coefficients that are negative or greater than 1 are also possible
#' because of negative values in the distribution. Compared to other measures of inequality,
#' the Gini coefficient is especially sensitive for changes in the middle of the distribution.
#'
#' Extension of the gini function in reldist package in order to handle missings.
#'
#' @param x a numeric vector containing at least non-negative elements.
#' @param weights an optional vector of weights of x to be used in the computation of the Gini
#' coefficient. Should be NULL or a numeric vector.
#'
#' @return The value of the Gini coefficient.
#'
#' @examples
#' #calculate Gini coefficient using Mexican Income data set
#' data(mex_inc_2008)
#'
#' #unweighted Gini coefficient:
#' gini.wtd(mex_inc_2008$income)
#'
#' #weighted Gini coefficient:
#' gini.wtd(x=mex_inc_2008$income, weights=mex_inc_2008$factor)
#'
#' @source Handcock, M. (2016), Relative Distribution Methods. Version 1.6-6. Project home page
#' at http://www.stat.ucla.edu/~handcock/RelDist.
#'
#' @references
#' Haughton, J. and S. Khandker. (2009) \emph{Handbook on poverty and inequality},
#' Washington, DC: World Bank.
#'
#' Cowell F. (2000) Measurement of Inequality. In Atkinson A. and Bourguignon F. (eds.) \emph{
#' Handbook of Income Distribution}. Amsterdam: Elsevier, p. 87-166.
#'
#' @export

gini.wtd <- function (x, weights = NULL)
{
  if (is.null(weights)){
    weights <- rep(1, length(x))}

  missing <- !(is.na(x) | is.na(weights))
  x <- x[missing]
  weights <- weights[missing]
  if (!all(weights>=0)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0)) stop("All weights are zero", call.=FALSE)
  weights <- weights/sum(weights)

  order <- order(x)
  x <- x[order]
  weights <- weights[order]
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
  return (gini)
}
