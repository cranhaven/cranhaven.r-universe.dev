#' Summarize eof-mapped alphas
#'
#' This function computes approximate normal intervals, etc. for fitted 
#' eof-mapped alphas.
#'
#' @export
#' 
#' @importFrom stats qnorm
#' 
#' @param eof_alpha structure containing posterior inference for transformed 
#'   remote coefficients
#' @param prob confidence level for confidence intervals and significance
#' @param coords.s matrix with coordinates where responses were 
#'  observed (lon, lat)
#'  
#' @examples
#' 
#' data("coprecip.predict")
#' attach(coprecip.predict)
#' 
#' alpha.eof.90 = summariseEOFAlpha(eof_alpha = eof_alpha_knots, prob = .9, 
#'   coords.s = coords.s)
#' 

summariseEOFAlpha = function( eof_alpha, prob=.95, coords.s ) {
  
  if(is.null(eof_alpha$est))
    eof_alpha$est = eof_alpha$eof_alpha
  
  n = nrow(coords.s)
  L = length(eof_alpha$est)/n # number of eof patterns
  
  # compute approximate normal intervals and significance
  z = qnorm((1-prob)/2, lower.tail = F)
  lower = eof_alpha$est - eof_alpha$sd*z
  upper = eof_alpha$est + eof_alpha$sd*z
  sig = apply(cbind(lower, upper), 1, function(x) {
    ifelse(prod(sign(x))==-1, F, T)
  })
  
  # report basic stats for alphas
  message(sum(sig), ' (', signif(mean(sig)*100, 3), 
          '%) significant eof-mapped teleconnection effects (', prob*100, 
          '% approximate normal intervals)')
  
  # compile and return information in a data frame
  data.frame(
    eof_alpha = eof_alpha$est,
    sd = eof_alpha$sd,
    lower = lower,
    upper = upper,
    signif = sig,
    posProb = eof_alpha$posProb,
    negProb = eof_alpha$negProb,
    pattern = 1:L,
    lon.Y = rep(coords.s[,1], rep(L,n)),
    lat.Y = rep(coords.s[,2], rep(L,n))
  )
}