#' Split conformal prediction intervals with Multivariate Response
#'
#' Compute prediction intervals using split conformal inference with multivariate
#' response.
#'
#' @param x The feature variables, a matrix n x p.
#' @param y The matrix of multivariate responses (dimension n x q)
#' @param x0 The new points to evaluate, a matrix of dimension n0 x p.
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
#' @param score The non-conformity measure. It can either be "max", "l2", "mahalanobis".
#' The default is "l2".
#' @param s.type The type of modulation function.
#'  Currently we have 3 options: "identity","st-dev","alpha-max". Default is "st-dev"
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
#' @return A list with the following components: x0,pred,k_s,s.type,s,alpha,randomized,tau,
#'   average_width,lo,up. In particular pred, lo, up are the matrices of
#'   dimension n0 x q, k_s is a scalar, s.type is a string, s is a vector of length q,
#'   alpha is a scalar between 0 and 1, randomized is a logical value,
#'   tau is a scalar between 0 and 1,and average_width is a positive scalar.
#'
#' @details If the two mad functions are provided they take precedence over the
#' s.type parameter,
#' and they force a local scoring via the mad function predicted values.
#'
#' @importFrom stats mad mahalanobis
#'
#' @importFrom utils flush.console
#'
#' @seealso \code{\link{conformal.multidim.full}}
#'
#' @references The s_regression and the "max" score are taken from
#' "Conformal Prediction Bands
#' for Multivariate Functional Data" by Diquigiovanni, Fontana, Vantini (2021).
#'
#' @example inst/examples/ex.split.R
#' @export conformal.multidim.split



conformal.multidim.split = function(x,y, x0, train.fun, predict.fun, alpha=0.1,
                                 split=NULL, seed=FALSE, randomized=FALSE,seed.rand=FALSE,
                                 verbose=FALSE, rho=0.5,
                                 score="l2",s.type="st-dev", mad.train.fun = NULL,
                                 mad.predict.fun = NULL) {


  ## Check Data and Splits
  check.split(x=x,y=y,x0=x0,train.fun=train.fun,
              predict.fun=predict.fun, alpha=alpha, seed=seed, training_size
              =rho, seed.rand=seed.rand, randomized=randomized,
              mad.train.fun = mad.train.fun, mad.predict.fun = mad.predict.fun,
              score = score)

  n=dim(x)[1]
  p=dim(x)[2]
  q=dim(y)[2]
  n0=nrow(x0)
  flag = FALSE #in general mad funs are not present

  # If mad funs exist, they take precedence
  if(is.function(mad.train.fun) && is.function(mad.predict.fun)){
    score = "identity"
    flag = TRUE
  }


  if (verbose == TRUE) txt = ""
  if (verbose != TRUE && verbose != FALSE) {
    txt = verbose
    verbose = TRUE
  }


  if(is.null(split)){

    if(ceiling(n*rho) !=n )
      m=ceiling(n*rho)
    else
      m=ceiling(n*rho)-1

    l=n-m

    if(seed!=FALSE){set.seed(seed)}

    training=sample(1:n,m)

  }

  else{
    training=split
  }

  calibration=setdiff(1:n,training)

  if(randomized==FALSE) {tau=1} else{
    if(seed.rand!=FALSE){set.seed(seed.rand)}
    tau=stats::runif(n=1,min=0,max=1)
  }

  check.tau(alpha=alpha,tau=tau,l=l)


  ###### TRAINING & RESIDUALS COMPUTATION
  if (verbose) {
    cat(sprintf("%sComputing models on first part ...\n",txt))
  }


  out = train.fun(x[training,],y[training,])
  fit = predict.fun(out,x)
  pred = predict.fun(out,x0)

  if (verbose) {
    cat(sprintf("%sComputing residuals and quantiles on second part ...\n",txt))
  }


  res = y - fit
  s=computing_s_regression(mat_residual=res[training,],type=s.type,
                                 alpha=alpha,tau=tau)
  resc = t(t(res[calibration,] )/ s)

  if(flag){ # with mad
    mad.out = mad.train.fun(x[training,],res[training,])
    resc = resc / mad.predict.fun(mad,out,x[calibration,])
    mad.x0 = mad.predict.fun(mad.out,x0)
  }

  switch(score,
          "max"={rho=apply(resc,1,function(x) max(abs(x)))},
           "l2" = {rho=rowSums(resc^2)},
           "mahalanobis" = {rho = mahalanobis(resc,colMeans(resc),cov(resc),tol=1e-2)}
           )


  k_s=sort(rho,decreasing=FALSE)[ceiling(l+tau-(l+1)*alpha)]
  average_width = mean(2*k_s*s)

  if(flag)
    band = mad.x0 * k_s
  else
    band=k_s*matrix(rep(s,n0),nrow = n0,byrow = TRUE)

  up=pred+band
  lo=pred-band


  return(structure(.Data=list(pred,lo,up,x0),
                   names=c("pred","lo", "up","x0")))

}

utils::globalVariables(c("pval", "seed.rand"))
