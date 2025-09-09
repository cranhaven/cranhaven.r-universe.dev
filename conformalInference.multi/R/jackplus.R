#' Multivariate Response Jackknife + Prediction Regions
#'
#' Compute prediction regions using multivariate Jackknife + inference.
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
#' @return A list with length n0, giving the lower and upper bounds for each observation.
#'
#' @details The work is an extension of the univariate approach to jackknife +
#'  inference to a multivariate context, exploiting the concept of depth measures.
#' @details This function is based on the package future.apply to
#'  perform parallelisation. If this package is not installed, then the function
#'  will abort.
#'
#' @example inst/examples/ex.jackplus.R
#' @export conformal.multidim.jackplus

conformal.multidim.jackplus = function(x,y,x0, train.fun, predict.fun, alpha=0.1) {

  ## Check Data and Splits
  check.split(x=x,y=y,x0=x0,train.fun=train.fun,
              predict.fun=predict.fun, alpha=alpha)

  n=dim(x)[1]
  p=dim(x)[2]
  q=dim(y)[2]
  n0=nrow(x0)

  ### Parallel sessions
  future::plan(future::multisession)
  options(future.rng.onMisuse="ignore")

  ### Train and fit the full model
  out = train.fun(x,y)
  fit = predict.fun(out,x)
  pred = predict.fun(out,x0)

  ## Compute models without each observation
  updated_models = future.apply::future_lapply(1:n,function(jj){
    mod_jj = train.fun(x[-jj,],y[-jj,])
    return(mod_jj)})

  ## Compute LOO residuals
  Loo<-t(future.apply::future_sapply(1:n, function(jj)
    abs(predict.fun(updated_models[[jj]] ,matrix( x[jj,],nrow = 1))-y[jj,])))


  ## Fitted values of LOO models
  fitted=future.apply::future_lapply(1:n0, function(i)
    t(future.apply::future_sapply(1:n,function(k)
      predict.fun(updated_models[[k]],matrix(x0[i,],nrow=1)))))


  joint<-future.apply::future_lapply(1:n0, function(i){
    return(rbind(fitted[[i]]-Loo,fitted[[i]]+Loo))
  })

  ## Compute depth
  joint.dep<-t(sapply(1:n0, function(k) depth.max(joint[[k]]) ))

  join <- future.apply::future_lapply(1:n0, function (j) {
    o=order(joint.dep[j,])
    qt=o[floor(alpha*n):n]
    obs<-joint[[j]][qt,]
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


