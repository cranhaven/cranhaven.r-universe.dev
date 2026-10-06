#' @title Mean log deviation
#'
#' @description Returns the (optional weighted) mean log deviation for a vector.
#'
#' @details The mean log deviation is a measure of inequality among values of a distribution. It is
#' a member of the Generalized Entropy Measures. Also referred to as GE(0). A value of zero is the
#' lowest possible inequality. The measure does not have an upper bound for the highest inequality.
#' It uses a logarithmic transformation of the values of the distribution. Therefore it
#' cannot handle negative or zero values. Those are excluded from the computation in this function.
#' The mean log deviation is more sensitive for changes in the lower tail of the distribution.
#'
#' Extension of the calcGEI function in IC2 package in order to handle missings.
#'
#' @param x a numeric vector containing at least non-negative elements.
#' @param weights an optional vector of weights of x to be used in the computation of the mean log
#' deviation. Should be NULL or a numeric vector.
#'
#' @return the value of the mean log deviation index.
#'
#' @examples
#' #calculate mean log deviation using Mexican Income data set
#' data(mex_inc_2008)
#'
#' #unweighted mean log deviation:
#' mld.wtd(mex_inc_2008$income)
#'
#' #weighted mean log deviation:
#' mld.wtd(x=mex_inc_2008$income, weights=mex_inc_2008$factor)
#'
#' @source
#' Plat, D. (2012). IC2: Inequality and Concentration Indices and Curves. R package
#' version 1.0-1. https://CRAN.R-project.org/package=IC2
#'
#' @references
#' Haughton, J. and S. Khandker. (2009) \emph{Handbook on poverty and inequality},
#' Washington, DC: World Bank.
#'
#' Cowell F. (2000) Measurement of Inequality. In Atkinson A. and Bourguignon F. (eds.) \emph{
#' Handbook of Income Distribution}. Amsterdam: Elsevier, p. 87-166.
#'
#' @export

mld.wtd <- function(x,weights=NULL)
{
  if (is.null(weights)){
    weights <- rep(1, length(x))}

  missing <- !(is.na(x) | is.na(weights))
  x <- x[missing]
  weights <- weights[missing]
  if (!all(weights>=0)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0)) stop("All weights are zero", call.=FALSE)

  x_sel <- x[x>0]
  weights_sel <- weights[x>0]
  weights_sel <- weights_sel/sum(weights_sel)
  mean <- stats::weighted.mean(x_sel,weights_sel)
  x_sel <- x_sel/mean
  mld <- (-sum(weights_sel*log(x_sel)))
  return(mld)
}
