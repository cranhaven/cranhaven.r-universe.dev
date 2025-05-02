#' Variable Selection via Thresholded Partial Correlation
#'
#' These are the main selection functions with fixed significance level \code{s} and \code{constant}.
#' The function \code{TPC} implements the thresholded partial correlation (TPC) approach to selecting important 
#' variables in linear models of Li et al. (2017).
#' The function \code{TPC_pl} implements the thresholded partial correlation approach to selecting important 
#' variables in partial linear models of Liu et al. (2018). 
#' This function also extends the PC-simple algorithm of Bühlmann et al. (2010) to partial linear models.
#'
#' @param y response vector;
#' @param x covariate matrix;
#' @param u non-parametric variable, should be a vector;
#' @param s \code{s} is a numeric value or vector that used as the significance level(s) for the partial correlation tests
#' @param constant a value that used as the tuning constant for partial
#' correlation test. \code{constant} is treated as 1 when method is "simple".
#' @param method the method to be used; default set as method = "threshold";
#'  "simple" is also available.
#' @param ... smoothing parameters and functions: \code{kernel}, \code{degree}, and bandwidth \code{h}.
#'
#' @return TPC.object a TPC object, which extends the \code{lm} object. New attributes are:
#' \itemize{
#'   \item beta - the fitted coefficients
#'   \item selected_index - the selected coefficients indices
#' }
#'
#' @examples
#' #generate partial linear data
#' samples <- generate_toy_pldata()
#' y <- samples[[1]]
#' x <- samples[[2]]
#' times <- samples[[3]]
#'
#' #perform variable selection via partial correlation
#' TPC.fit = TPC_pl(y,x,times,0.05,1,method="threshold")
#' TPC.fit$beta
#'
#' @import stats
#' @importFrom utils combn
#' @export
TPC_pl <- function(y, x, u = NULL, s = 0.05, constant = 1, method = "threshold", ...) {

  if(is.null(u)){
    TPC_ojb <- TPC(y,x,s,constant,method)
    return(TPC_ojb)
  }else{
    #validate inputs of u
    stopifnot(is.numeric(u),is.vector(u))

    #smooth y and x#deal with ...
    args_list <- list(...)
    if (is.null(args_list$degree)) args_list$degree <- degree <- 1
    if (is.null(args_list$kernel)) args_list$kernel <- kernel <- function(x) exp(-x^2/2)/(2*2*pi)
    tgrid <- seq(from=min(u),to=max(u),length=length(u))
    yres <- y - yprediction(u,degree,y,u,kernel)
    xres <- x
    for (i in (1:NCOL(x))){
      xres[,i] <- x[,i] - yprediction(u, degree, x[,i], u, kernel)
    }

    #TPC on smoothed y and x
    TPC_ojb <- TPC(yres,xres,s,constant,method)
    betahat_tpc <- TPC_ojb$beta
    newy <- y-x%*%betahat_tpc
    hatalpha_tpc <- yprediction(tgrid, degree, newy, u, kernel)
    TPC_ojb$uhat <- hatalpha_tpc
    #TPC_ojb$ugrid <- ugrid
    return(TPC_ojb)

    }


}
