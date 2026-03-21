#' Test iid samples for correct cdf using KS test
#'
#' Test if samples are behaving like an iid sample from a given CDF
#' via the KS test and a sequential approach. Only works for
#' continuous CDFs. Will report a warning if values are discrete
#'
#' @param object A function taking one argument - that generates n univariate iid samples.
#' @param cdf A univariate cumulative distribution function, taking exactly one argument.
#' @inheritParams expect_mc_test
#' @examples
#'    sampler <- function(n) rnorm(n)
#'    expect_mc_iid_ks(sampler, pnorm)
#' @return The first argument, invisibly, to allow chaining of expectations.
#' @export
expect_mc_iid_ks <- function(object, cdf, control=NULL) {

  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    
  pvalsampler <- function(n){
      xall <- object(n)
      stats::ks.test(xall,cdf)$p.value
  }
  
  expect_mc_test(pvalsampler,control)
  invisible(act$val)   
}

#' Test iid samples for correct cdf using chisq test
#'
#' Test if samples are behaving like an iid sample from a given distribution
#' via the chisq test and a sequential approach. Only works for
#' discrete distributions taking finitely many values.
#'
#' @param object A function taking one argument - that generates n univariate iid samples.
#' @param prob A vector of probabilities for finitely many consecutive integers from 0 onward.
#' @inheritParams expect_mc_test
#' @examples
#'    sampler <- function(n) rbinom(n,prob=0.6,size=5)
#'    expect_mc_iid_chisq(sampler, dbinom(0:5,prob=0.6,size=5))
#'    testthat::expect_error(expect_mc_iid_chisq(sampler, dbinom(0:5,prob=0.63,size=5)))
#' 
#' @return The first argument, invisibly, to allow chaining of expectations.
#' @export
expect_mc_iid_chisq <- function(object, prob, control=NULL) {

  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    
  pvalsampler <- function(n){
      x <- object(n)
      tab <- tabulate(x+1,nbins=length(prob))
      stats::chisq.test(tab,p=prob)$p.value
  }
    
  expect_mc_test(pvalsampler,control)
    
  invisible(act$val)   
}

#' Test iid samples for correct mean
#'
#' Test if samples are coming from a specific mean. Not guaranteed to
#' be exact, as it estimates the standard error from the sample.
#'
#' @param object A function taking one argument - that generates n univariate iid samples.
#' @param mean The expected mean of the samples returned from object.
#' @inheritParams expect_mc_test
#' @examples
#'    sampler <- function(n) rbinom(n,prob=0.6,size=5)
#'    expect_mc_iid_mean(sampler, mean=3)
#'    testthat::expect_error(expect_mc_iid_mean(sampler, mean=2))
#'
#' @return The first argument, invisibly, to allow chaining of expectations.
#' @export
expect_mc_iid_mean <- function(object, mean, control=NULL) {

  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    
  pvalsampler <- function(n){
      x <- object(n)
      z=abs((base::mean(x)-mean)/(stats::sd(x)/sqrt(n)))
      2*stats::pnorm(z,lower.tail=FALSE)
  }
    
  expect_mc_test(pvalsampler,control)

  invisible(act$val)
}



#' Test if p-values are coming from the null using a sequential
#' approach.
#'
#' Requires as input a generic test that for a given sample size
#' provides a vector of p-values. Aims to reject if these are not from
#' the null. Guarantees a bound on the type I error rate.
#'
#' @param object A function taking one argument n  - that generates p-values based on a sample size n.
#' @param control a list controlling the algorithm
#' * n number of samples to be taken in the first step. Default: 1e3
#' * maxseqsteps: Number of sequential attempts to use. Default: 7.
#' * incn: Factor by which to multiply n from the second  sequential attempt onward. Default: 4.
#' * level: bound on the type I error, ie the probability of wrongly rejecting a sampler with the correct distribution. Default: 1e-5.
#' * debug: If positive  then debug information will be printed via 'message()'. Default: 0.
#'@param npval number of p-values returned by the test. A Bonferroni correction is applied if >1. Default: 1.
#' @examples
#' pvalsampler <- function(n){
#'       x <- sample.int(11,size=n,replace=TRUE)-1;
#'      chisq.test(tabulate(x+1,nbins=11),
#'                 p=rep(1/11,11))$p.value
#' }
#' expect_mc_test(pvalsampler)
#' 
#' @return The first argument, invisibly, to allow chaining of expectations.
#' @export
expect_mc_test <- function(object, control=NULL, npval=1) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  if (is.null(control))
      control <- list()
  if (is.null(control$n)) control$n <- 1e3
  if (is.null(control$level)) control$level <- 1e-5;
  if (is.null(control$maxseqsteps)) control$maxseqsteps <- 7;
  if (is.null(control$incn)) control$incn <- 4;
  if (is.null(control$debug)) control$debug <- 0
  
  beta <- control$level/control$maxseqsteps
  gamma <- beta^(1/control$maxseqsteps)
  if (control$debug>0){
      message("beta=",format(beta)," gamma=",format(gamma),sep="")
  }
  
  for (j in 1:control$maxseqsteps){
      ##sample
      act$p.value <- object(control$n)
      if (length(act$p.value)!=npval){
          testthat::fail(paste("dimension of p-value vector (",length(act$p.value),") different from npval (",npval,")",sep=""))
          return(invisible(act$val))
      }
      if (control$debug>0){
          message("Iteration ",j,": p-val=",format(act$p.value)," beta=",format(beta),sep="")
      }
      
      if (min(act$p.value)*npval<=beta){
          if (control$debug>0){
              message("Discrepancy detected.")
          }
          if (npval>1){
              wfail <- which(act$p.values*npval<=beta)
              message <- paste("Test failed for components ", paste(wfail,collapse=",")," with p-values=",paste(format(act$p.values[wfail]),collapse=",")," in iteration ",j,"\n",sep="")
          }else{
              message <- paste("Test failed with p-value=",paste(format(act$p.value),collapse=",")," in iteration ",j,"\n",sep="")
          }
          testthat::fail(message)
          return(invisible(act$val))
      }
      if (min(act$p.value)*npval>gamma+beta)
          break;
      beta <- beta/gamma
      if (j==1) control$n=ceiling(control$n*control$incn)
  }
  if (control$debug>0){
      message("No discrepancy detected.")
  }
  
  testthat::succeed()
  invisible(act$val)
}

    
