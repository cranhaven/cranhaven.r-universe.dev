#' @title Bayesian Information Criterion (BIC) for a partition.
#'
#' @description This function calculates an extended version of BIC, which is computed using a particular weighted average of the total residual sum of squares and the number of clusters.
#'
#' SCEM uses the following equation for the BIC of each partition:
#'
#' \loadmathjax
#' \mjdeqn{BIC(P) = (np)\log \left\lbrace\frac{RSS(P)}{np}\right\rbrace + |P|(B_{n}^{-1}-1) \log(nB_{n}),}{ASCII representation}
#'
#' where \mjeqn{RSS(P) = \sum_{q=1}^{Q} RSS(S_q)}{ASCII representation}.
#'
#' The sample size of each individual time series (i.e. the number of observations) is denoted by \eqn{n}, but in dealing with archaeological data, not all the time series in a data set will have the same number of observations.
#'
#' In order to have a reasonable representative value for the sample size, we have chosen to use the natural arithmetic mean \mjeqn{n=(n_1+\dots+n_p)/p}{ASCII representation}.
#'
#' \mjeqn{(B_{n}^{-1}-1)\log(nB_{n})}{ASCII representation} is the tuning parameter that places the penalty on the number of clusters (also note that the term \mjeqn{nB_{n}}{ASCII representation}).
#' Using a different tuning parameter \mjeqn{\gamma_{n}}{ASCII representation} in place of \mjeqn{(B_{n}^{-1}-1)\log(nB_{n})}{ASCII representation} allows stronger or weaker penalties on the number of clusters.
#'
#' @param paths A list of data frames, where each frame contains the data for one individual. Every
#' data frame should have two columns with names 'distance' and 'oxygen'.
#'
#' @param partition A list of vectors. Each element in the list is a vector of integers, corresponding
#' to individuals considered in one group.
#'
#' @param bandwidth Denotes the order of the bandwidth that should be used in the estimation process.
#' bandwidth = k will mean that the bandwidth is n^k.
#'
#' @export
#'
#' @return Value of the extended BIC function for the partition.
#'
#' @examples
#' armenia_split = split(armenia,f = armenia$ID)
#' band = -0.33
#' p = length(armenia_split)
#' EBIC(armenia_split,1:p,band)

EBIC <- function(paths,
                 partition,
                 bandwidth){

  for(i in 1:length(paths)){
    if (!any(colnames(paths[[i]])==c("distance","oxygen"))) {stop('data frame does not contain distance and oxygen columns')}
  }
  if (! is.atomic(bandwidth) || !length(bandwidth)==1) {stop('bandwidth needs to be a single value')}
  for(i in 1:length(paths)){if (any(is.na(paths[[i]]))) {stop('Data has NAs')}}

  q = length(partition)
  p = length(paths)
  size = as.numeric(lapply(paths,nrow))
  n = mean(size)
  bn = n^{bandwidth}
  total = 0
  for (i in 1:q){
    RSS = sum(calculateRSS(paths,partition[[i]],bandwidth))
    total = total+RSS
  }
  out = n*p*log(total/(n*p))+q*log(n*bn)*(1/bn-1)

  return(out)

}
