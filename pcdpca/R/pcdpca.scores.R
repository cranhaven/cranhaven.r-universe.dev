#' Compute periodically correlated DPCA scores, given the filters XI
#'
#' @param X multivariate time series
#' @param XI series of filters returned from pcdpca
#' @keywords pcdpca
#' @export
pcdpca.scores <- function(X, XI)
{
  period = XI$period
  Y = freqdom::filter.process(pc2stat(X,period=period), XI)
  stat2pc(Y,period=period,n=nrow(X))
}


