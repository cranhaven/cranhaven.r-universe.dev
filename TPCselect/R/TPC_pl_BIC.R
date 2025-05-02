#' Variable Selection via Thresholded Partial Correlation
#'
#' Use BIC to select the best \code{s} and \code{constant} over grids.
#'
#' @param y response vector;
#' @param x covariate matrix;
#' @param u non-parametric variable, should be a vector;
#' @param s a value or a vector that used as significance level(s) for partial
#' correlation test. BIC will be used to select the best \code{s}.
#' @param constant a value or a vector that used as the tuning constant for partial
#' correlation test. BIC will be used to select the best \code{constant}.
#' \code{constant} is treated as 1 when method is "simple".
#' @param method the method to be used; default set as method = "threshold";
#'  "simple" is also available.
#' @param ... smoothing parameters and functions: \code{kernel}, \code{degree}, and bandwidth \code{h}.
#'
#'
#' @return TPC.object a TPC object, which extends the \code{lm} object. New attributes are:
#' \itemize{
#'   \item beta - the fitted coefficients
#'   \item selected_index - the selected coefficients indices
#' }
#'
#' @examples
#'
#' #generate partial linear data
#' samples <- generate_toy_pldata()
#' y <- samples[[1]]
#' x <- samples[[2]]
#' times <- samples[[3]]
#'
#' #perform variable selection via partial correlation
#' TPC.fit = TPC_pl_BIC(y,x,times,0.05,c(1,1.5),method="threshold")
#' TPC.fit$beta
#'
#'
#' @export
#'
TPC_pl_BIC <- function(y, x, u = NULL, s = 0.05, constant = 1, method = "threshold", ...) {

  p <- NCOL(x)
  if (method == "simple") {
    constant = c(1)
  } #ignore constant when using simple algorithm

if(is.null(u)){
    TPC_ojb <- TPC_BIC(y,x,s,constant,method)
    return(TPC_ojb)
  }else{
    #validate inputs of u
    stopifnot(is.numeric(u),is.vector(u))

    #smooth y and x
    degree = 1 # change this to ...
    kernel <- function(x) exp(-x^2/2)/(2*2*pi)
    tgrid <- seq(from=min(u),to=max(u),length=length(u))
    yres <- y - yprediction(tgrid,degree,y,u,kernel)
    xres <- x
    for (i in (1:NCOL(x))){
      xres[,i] <- x[,i] - yprediction(tgrid, degree, x[,i], u, kernel)
    }

    #TPC on smoothed y and x
    TPC_ojb <- TPC_BIC(yres,xres,s,constant,method)
    betahat_tpc <- TPC_ojb$beta
    newy <- y-x%*%betahat_tpc
    degree = 2 # change this to ...
    hatalpha_tpc <- yprediction(tgrid, degree, newy, u, kernel)
    TPC_ojb$uhat <- hatalpha_tpc
    return(TPC_ojb)

  }


}
