# wowa Package v4.0
#SEXP way Rcpp

library(Rcpp)


wowa <- function()
{
  # This function outputs a list of all functions included in this toolbox.
  
  print("The list of functions in wowa Tool Box:")
  
  print("wowa.OWA([inputs],[OWA weights])")
  print("wowa.WAM([inputs],[weights])")
  print("wowa.weightedf([inputs], [ Weights], [dimension], [n-variate function to extend],[tree depth])")
  print("wowa.weightedOWAQuantifierBuild([number of variables],[input weights], [Associated OWA Weights])")
  print("wowa.weightedOWAQuantifier([number of variables],[input weights], [Associated OWA Weights],[quantifier] )")
  print("wowa.ImplicitWOWA([inputs],[Weights], [Associated OWA Weights],[dimension])")
  print("wowa.WAn([inputs], [Weights]Associated OWA Weights],[dimension],[Bivariate function],[tree depth])")
}






wowa.WAM <- function(n, x, w) {
  return(.Call('WOWA_WAM', PACKAGE = 'wowa', n, x, w))  
}
wowa.OWA <- function(n, x, w) {
  return(.Call('WOWA_OWA', PACKAGE = 'wowa', n, x, w))  
}

#// OWA_OWA is defined in Rcppexports.cpp. same for the following functions

wowa.weightedf <- function(x, p, w, n, Fn, L) {
  return(.Call('WOWA_weightedf', PACKAGE = 'wowa', x, p, w, n, Fn, L  ))
}

wowa.weightedOWAQuantifierBuild <- function(p, w, n) {
  out<-.Call('WOWA_weightedOWAQuantifierBuild', PACKAGE = 'wowa', p, w, n)
  return (out)
}

wowa.weightedOWAQuantifier <- function(x, p, w, n, spl) {
  return( .Call('WOWA_weightedOWAQuantifier', PACKAGE = 'wowa', x, p, w, n, spl$spl, spl$Tnum))
}

wowa.ImplicitWOWA <- function(x, p, w, n) {
  return(.Call('WOWA_ImplicitWOWA', PACKAGE = 'wowa', x, p, w, n))
}

wowa.WAn <- function(x, w, n, Fn, L) {
  return(.Call('WOWA_WAn', PACKAGE = 'wowa', x, w, n, L, Fn))
}


