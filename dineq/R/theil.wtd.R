#' @title Theil index
#'
#' @description Returns the (optional weighted) Theil index for a vector.
#'
#' @details The Theil index is a measure of inequality among values of a distribution. It is a
#' member of the Generalized Entropy Measures. Also referred to as GE(1). The index can have a value between 0 and ln N
#' (the logarithm of the number of values), with 0 being the lowest possible inequality. It uses
#' a logarithmic transformation of the values of the distribution. Therefore it cannot
#' handle negative or zero values. Those are excluded from the computation in this function. The
#' Theil Index is more sensitive for changes in the upper tail of the distribution.
#'
#' Extension of the calcGEI function in IC2 package in order to handle missings.
#'
#' @param x a numeric vector containing at least non-negative elements.
#' @param weights an optional vector of weights of x to be used in the computation of the Theil
#' index. Should be NULL or a numeric vector.
#'
#' @return The value of the Theil index.
#'
#' @examples
#' #calculate Theil Index using Mexican Income data set
#' data(mex_inc_2008)
#'
#' #unweighted Theil Index:
#' theil.wtd(mex_inc_2008$income)
#'
#' #weighted Theil Index:
#' theil.wtd(x=mex_inc_2008$income, weights=mex_inc_2008$factor)
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

theil.wtd <- function(x,weights=NULL)
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
  theil <- (sum(weights_sel*x_sel*log(x_sel)))
  return(theil)
}


