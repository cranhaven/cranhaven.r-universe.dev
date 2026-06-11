#' Convenience Functions, Moving Window Statistics, and Graphics
#' 
#' Contains functions that do something convenient (e.g. create BMI categories), 
#' functions for calculating moving-window statistics efficiently, and functions 
#' for generating various figures (e.g. histograms with fitted probability 
#' mass/density function).
#' 
#' \tabular{ll}{
#' Package: \tab dvmisc \cr
#' Type: \tab Package \cr
#' Version: \tab 1.1.4 \cr
#' Date: \tab 2019-12-15 \cr
#' License: \tab GPL-3 \cr
#' }
#' 
#' See \href{https://cran.r-project.org/package=dvmisc}{CRAN documentation} for 
#' full list of functions. 
#' 
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#' 
#' @references 
#' Eddelbuettel, D. and Francois, R. (2011) Rcpp: Seamless R and C++ 
#' Integration. Journal of Statistical Software, 40(8), 1-18. 
#' \url{http://www.jstatsoft.org/v40/i08/}.
#' 
#' Eddelbuettel, D. (2013) Seamless R and C++ Integration with Rcpp. Springer, 
#' New York. ISBN 978-1-4614-6867-7.
#' 
#' Eddelbuettel, D. and Balamuta, J.J. (2017). Extending R with C++: A Brief 
#' Introduction to Rcpp. PeerJ Preprints 5:e3188v1. 
#' \url{https://doi.org/10.7287/peerj.preprints.3188v1}.
#' 
#' Acknowledgment: This material is based upon work supported by the 
#' National Science Foundation Graduate Research Fellowship under Grant No. 
#' DGE-0940903.
#' 
#' @importFrom dplyr %>%
#' @docType package
#' @import data.table
#' @import ggplot2
#' @import graphics
#' @importFrom MASS fitdistr
#' @importFrom mvtnorm dmvnorm
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @importFrom rbenchmark benchmark
#' @importFrom stats IQR anova aov binom.test chisq.test confint dbeta dbinom dcauchy dchisq dexp df dgamma dgeom dhyper dlnorm dnbinom dnorm dpois dt dunif dweibull fisher.test median nlminb prop.test pt qchisq qnorm qt quantile sd t.test var
#' @importFrom survey svyquantile svyby
#' @importFrom tab formatp
#' @importFrom utils head tail capture.output
#' @useDynLib dvmisc, .registration=TRUE
#' @name dvmisc
NULL