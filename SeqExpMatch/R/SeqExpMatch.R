#' SeqExpMatch
#' 
#' Generates the following sequential two-arm experimental designs
#' (1) completely randomized (Bernoulli)
#' (2) balanced completely randomized
#' (3) Efron's (1971) Biased Coin
#' (4) Atkinson's (1982) Covariate-Adjusted Biased Coin
#' (5) Kapelner and Krieger's (2014) Covariate-Adjusted Matching on the Fly
#' (6) Kapelner and Krieger's (2021) CARA Matching on the Fly with Weighted Covariates
#' (7) Kapelner and Krieger's (2021) CARA Matching on the Fly with Weighted Covariates Stepwise
#'
#' @name 		SeqExpMatch
#' @docType 	package
#' @title 		Sequential Experimental Designs via Matching On-the-Fly
#' @author 		Adam Kapelner \email{kapelner@@qc.cuny.edu}
#' @references 	Adam Kapelner and Abba Krieger A Matching Procedure for Sequential Experiments that Iteratively Learns which Covariates Improve Power, Arxiv 2010.05980 
#' @keywords 	design htest
#' @import      stats checkmate doParallel R6
##### Run "library(roxygen2); roxygenise("SeqExpMatch", clean = TRUE)" to regenerate all Rd files and NAMESPACE and DESCRIPTION file
##### but make sure you are in the root directory of the project
NULL