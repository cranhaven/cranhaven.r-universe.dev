#' Multi Split Conformal Prediction Regions with Multivariate Response
#'
#' Compute prediction intervals using Multi Split conformal inference with
#' multivariate response.
#'
#' @param x The feature variables, a matrix nxp.
#' @param y The matrix of multivariate responses (dimension nxq)
#' @param x0 The new points to evaluate, a matrix of dimension n0xp.
#' @param train.fun A function to perform model training, i.e., to produce an
#'   estimator of E(Y|X), the conditional expectation of the response variable
#'   Y given features X. Its input arguments should be x: matrix of features,
#'   and y: matrix of responses.
#' @param predict.fun A function to perform prediction for the (mean of the)
#'   responses at new feature values. Its input arguments should be out: output
#'   produced by train.fun, and newx: feature values at which we want to make
#'   predictions.
#' @param alpha Miscoverage level for the prediction intervals, i.e., intervals
#'   with coverage 1-alpha are formed. Default for alpha is 0.1.
#' @param split Indices that define the data-split to be used (i.e., the indices
#'   define the first half of the data-split, on which the model is trained).
#'   Default is NULL, in which case the split is chosen randomly.
#' @param seed Integer to be passed to set.seed before defining the random
#'   data-split to be used. Default is FALSE, which effectively sets no seed.
#'   If both split and seed are passed, the former takes priority and the latter
#'   is ignored.
#' @param randomized Should the randomized approach be used? Default is FALSE.
#' @param seed.rand The seed for the randomized version. Default is FALSE.
#' @param verbose Should intermediate progress be printed out? Default is FALSE.
#' @param rho Split proportion between training and calibration set.
#' Default is 0.5.
#' @param score The chosen score for the split conformal function.
#' @param s.type The type of modulation function.
#'  Currently we have 3 options: "identity","st-dev","alpha-max". Default is "std-dev"
#' @param B Number of repetitions. Default is 100.
#' @param lambda Smoothing parameter. Default is 0.
#' @param tau It is a smoothing parameter:
#' tau=1-1/B  Bonferroni intersection method
#' tau=0 unadjusted intersection
#' Default is 1-(B+1)/(2*B).
#' @param mad.train.fun A function to perform training on the absolute residuals
#'   i.e., to produce an estimator of E(R|X) where R is the absolute residual
#'   R = |Y - m(X)|, and m denotes the estimator produced by train.fun.
#'   This is used to scale the conformal score, to produce a prediction interval
#'   with varying local width. The input arguments to mad.train.fun should be
#'   x: matrix of features, y: vector of absolute residuals, and out: the output
#'   produced by a previous call to mad.train.fun, at the \emph{same} features
#'   x. The function mad.train.fun may (optionally) leverage this returned
#'   output for efficiency purposes. See details below. The default for
#'   mad.train.fun is NULL, which means that no training is done on the absolute
#'   residuals, and the usual (unscaled) conformal score is used. Note that if
#'   mad.train.fun is non-NULL, then so must be mad.predict.fun (next).
#' @param mad.predict.fun A function to perform prediction for the (mean of the)
#'   absolute residuals at new feature values. Its input arguments should be
#'   out: output produced by mad.train.fun, and newx: feature values at which we
#'   want to make predictions. The default for mad.predict.fun is NULL, which
#'   means that no local scaling is done for the conformal score, i.e., the
#'   usual (unscaled) conformal score is used.
#'
#' @return A list with length n0, giving the lower and upper bounds for each observation.
#'
#' @details The work is an extension of the univariate approach to Multi Split
#' conformal inference to a multivariate context, exploiting the concept of depth measure.
#'
#' @details This function is based on the package \code{\link{future.apply}} to
#'  perform parallelization.
#'
#' @references "Multi Split Conformal Prediction" by Solari, Djordjilovic (2021) <arXiv:2103
#'.00627> is the baseline for the univariate case.
#'
#' @example inst/examples/ex.msplit.R
#' @export conformal.multidim.msplit



conformal.multidim.msplit = function(x,y, x0, train.fun, predict.fun, alpha=0.1,
                                      split=NULL, seed=FALSE, randomized=FALSE,
                                      seed.rand=FALSE,
                                      verbose=FALSE, rho=NULL,score = "max",
                                      s.type = "st-dev",B=100,lambda=0,
                                      tau = 0.1,mad.train.fun = NULL,
                                      mad.predict.fun = NULL) {



  if(is.null(rho) || length(rho)!=B)
    rho=rep(0.5,B)

  if (!is.null(seed)) set.seed(seed)
  check.pos.num(lambda)
  check.num.01(tau)

  n0=nrow(x0)
  p=ncol(x0)
  q=ncol(y)
  n=nrow(x)
  full=q*n0
  loB<-upB<-matrix(0,nrow=B,ncol=full)

  tr = 2*tau*B + .001


  future::plan(future::multisession)
  options(future.rng.onMisuse="ignore")

  ## Run B times the split algorithm
  lo_up <- future.apply::future_lapply(1:B, function(bbb) {


    out<-conformal.multidim.split(x,y, x0, train.fun, predict.fun,
                                  alpha*(1-tau) + (alpha*lambda)/B,
                                  split, seed+bbb, randomized,seed.rand,
                                  verbose, rho[bbb] ,score, s.type,mad.train.fun,
                                  mad.predict.fun)




    return(rbind(out$lo,out$up))

  })

  ## Reshape as needed
  mat<-do.call(rbind, lo_up)
  joint<-lapply(1:n0, function(u) mat[seq(u,nrow(mat),n0),])

  ## Compute depth
  joint.dep<-t(sapply(1:n0, function(u) 1/apply(abs(scale(joint[[u]])),1,max)))


  ## Select the deeper level set according to 'tr' and get lower and upper bounds
  join <- future.apply::future_lapply(1:n0, function (j) {
    o=order(joint.dep[j,],decreasing = TRUE)
    a<-floor(tr)
    qt=o[1:a]
    obs<-joint[[j]][qt,]
    lo<-apply(obs,2,min)
    up<-apply(obs,2,max)
    return(obs)
  })

  lo<-t(sapply(1:n0, function(i){
    return(apply(join[[i]],2,min))
  }))

  up<-t(sapply(1:n0, function(i){
    return(apply(join[[i]],2,max))
  }))


  ## To avoid CRAN check errors
  ## R CMD check: make sure any open connections are closed afterward
  future::plan(future::sequential)



  return(list(lo=lo,up=up,x0=x0))
}

