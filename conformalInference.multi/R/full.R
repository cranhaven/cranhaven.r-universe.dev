#' Full Conformal Prediction Regions, Multivariate Response
#'
#' Compute prediction intervals using full conformal inference with multivariate
#' response
#'
#' @param x Matrix of features, of dimension (say) n x p.
#' @param y Matrix of responses, of length (say) n X q.
#' @param x0 Matrix of features, each row being a point at which we want to
#'   form a prediction interval, of dimension (say) n0 x p.
#' @param train.fun A function to perform model training, i.e., to produce an
#'   estimator of E(Y|X), the conditional expectation of the response variable
#'   Y given features X. Its input arguments should be x: matrix of features,
#'   y: vector of responses, and out: the output produced by a previous call
#'   to train.fun, at the \emph{same} features x. The function train.fun may
#'   (optionally) leverage this returned output for efficiency purposes. See
#'   details below.
#' @param predict.fun A function to perform prediction for the (mean of the)
#'   responses at new feature values. Its input arguments should be out: output
#'   produced by train.fun, and newx: feature values at which we want to make
#'   predictions.
#' @param alpha Miscoverage level for the prediction intervals, i.e., intervals
#'   with coverage 1-alpha are formed. Default for alpha is 0.1.
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
#' @param score Method to compute nonconformity measure in the multivariate regime.
#' The user can choose between squared l^2 norm of the residual,
#' mahalanobis depth of the residual, the max norm of the residual.
#' @param s.type The type of modulation function.
#'  Currently we have 3 options: "identity","st-dev". Default is "st-dev"
#' @param num.grid.pts.dim Number of grid points per dimension used when forming the conformal
#'   intervals (each num.grid.pts.dim^q points is a trial point). Default is
#'   100.
#' @param grid.factor Expansion factor used to define the grid for the conformal
#'   intervals, i.e., the grid points are taken to be equally spaced in between
#'   -grid.factor x max(abs(y)) and grid.factor x max(abs(y)). Default is 1.25. In
#'   this case (and with exchangeable data, thus unity weights) the restriction
#'   of the trial values to this range costs at most 1/(n+1) in coverage. See
#'   details below.
#' @param verbose Should intermediate progress be printed out? Default is FALSE.
#'
#' @return A list with the following components: pred, valid_points. The first
#'   is a matrix of dimension n0 x q, while the second is a list of length n0, containing in each
#'   position a matrix of varying number of rows (depending on which points where accepted by
#'   the method) and with a number of columns equal to q + 1. Indeed, valid_points contains the
#'   selected points on the y-grid as well as the p-values.
#'
#' @details Due to eventual computational overload the function is restricted to a bivariate y.
#' @details This function is based on the package \code{\link{future.apply}} to
#'  perform parallelisation.
#'
#' @details If the data (training and test) are assumed to be exchangeable, the basic
#'   assumption underlying conformal prediction, then the probability that a new
#'   response value will lie outside of (-max(abs(y)), max(abs(y))), where y is
#'   the vector of training responses, is 1/(n+1).  Thus the restriction of the
#'   trials values to (-grid.factor x max(abs(y)), grid.factor x max(abs(y))), for
#'   all choices grid.factor >= 1, will lead to a loss in coverage of at most
#'   1/(n+1). This was also noted in "Trimmed Conformal Prediction for
#'   High-Dimensional Models" by Chen, Wang, Ha, Barber (2016) <arXiv:1611.09933>
#'   (who use this basic fact as motivation for proposing more refined trimming methods).
#'
#' @seealso \code{\link{conformal.multidim.split}}
#'
#' @example inst/examples/ex.full.R
#'
#' @export conformal.multidim.full


conformal.multidim.full = function(x, y, x0, train.fun, predict.fun,alpha = 0.1,mad.train.fun = NULL,
                                mad.predict.fun = NULL,
                                score='l2', s.type = "st-dev",
                                num.grid.pts.dim=100, grid.factor=1.25,
                                verbose=FALSE) {

  # Set up data
  x = as.matrix(x)
  y = as.matrix(y)
  q = ncol(y)
  n = nrow(x)
  p = ncol(x)
  x0 = matrix(x0,ncol=p)
  n0 = nrow(x0)
  valid_points = vector("list",n0)
  check.full(x,y,x0,train.fun,predict.fun, alpha, num.grid.pts.dim,grid.factor,score,
             mad.train.fun, mad.predict.fun)

  if (length(num.grid.pts.dim) != 1 || !is.numeric(num.grid.pts.dim)
      || num.grid.pts.dim <= 1 || num.grid.pts.dim >= 1000
      || round(num.grid.pts.dim) != num.grid.pts.dim) {
    stop("num.grid.pts must be an integer between 1 and 1000")
  }
  check.pos.num(grid.factor)

  flag = FALSE #in general mad funs are not present

  # If mad funs exist, they take precedence
  if(is.function(mad.train.fun) && is.function(mad.predict.fun)){
    score = "identity"
    flag = TRUE
  }

  # Users may pass in a string for the verbose argument
  if (verbose == TRUE) txt = ""
  if (verbose != TRUE && verbose != FALSE) {
    txt = verbose
    verbose = TRUE
  }

  if (verbose) cat(sprintf("%sInitial training on full data set ...\n",txt))

  if(! s.type %in% c("identity","st-dev"))
    stop("Please provide a valid value for the argument 's.type'. \n
         Choose between 'identity' or 'st-dev' ! \n")

  # Train, fit, and predict on full data set
  out = train.fun(x,y)
  fit = matrix(predict.fun(out,x),nrow=n)
  pred = matrix(predict.fun(out,x0),nrow=n0)

  # Trial values for y, empty lo, up matrices to fill
  ymax = apply(abs(y),2,max)
  y_marg = lapply(ymax,function(value)
    {seq(-grid.factor*value, grid.factor*value,length=num.grid.pts.dim)})
  yvals = as.matrix(expand.grid(y_marg))
  pvals = vector(mode='numeric',length=num.grid.pts.dim^q)
  xx = rbind(x,rep(0,p))


  future::plan(future::multisession)
  options(future.rng.onMisuse="ignore")


  for(k in 1:n0){

    xx[n+1,] = x0[k,]

  # Refit for each point in yvals, compute conformal p-value
    pvals = future.apply::future_sapply (1:(num.grid.pts.dim^q), function (j) {

      if (verbose) {
        cat(sprintf("\r%sProcessing grid point %i (of %i) ...",txt,j,num.grid.pts.dim^q))
        flush.console()
      }

      yy = rbind(y,yvals[j,])

      if (j==1)
        out = train.fun(xx,yy)
      else
        out = train.fun(xx,yy,out)

      r = yy - matrix(predict.fun(out,xx),nrow=n+1)

      if (flag) {
          if (j==1)
            out.mad = mad.train.fun(xx,r)
          else
            out.mad = mad.train.fun(xx,r,out.mad)
            r = r / mad.predict.fun(out.mad,xx)
      }

      s=computing_s_regression(mat_residual=r,type=s.type,
                               alpha=alpha,tau=0)
      r = t(t(r)/ s)

      switch(score,
           "l2"={ncm=rowSums(r^2)},
           "mahalanobis"={ncm=mahalanobis(r,colMeans(r),cov(r))},
           "max"={ncm=apply(abs(r), 1, max)}
      )

      return(sum(ncm>=ncm[n+1])/(n+1))

  })
    pval_matrix=data.frame(cbind(yvals,pval=pvals))
    valid_points[[k]] = data.frame(pval_matrix[which(pvals > alpha),])
  }


  ## To avoid CRAN check errors
  ## R CMD check: make sure any open connections are closed afterward
  future::plan(future::sequential)

  return(list(valid_points = valid_points,pred = data.frame(pred)))
}


