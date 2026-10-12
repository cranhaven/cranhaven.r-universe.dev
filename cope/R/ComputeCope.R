#' Compute CoPE sets.
#' 
#' Computes CoPE sets for the data Y using the algorithm from Sommerfeld, Sain
#' and Schwartzman (2015).
#' 
#' The \code{V} argument is a 4-dimensional array containing the covariance 
#' matrices associated with \code{Z$z}.  Specifically, \code{V[i,j,,]} is the 
#' covariance matrix of the data in \code{Z$z[i,j,]}.  If \code{V} is specified, 
#' then the covariance matrix in each element of the array is used to transform 
#' \code{X} and the appropriate element of \code{Z$z} before fitting the linear 
#' model.  This is used in place of estimating the covariance matrix withing the 
#' \code{nlme::gls} function.   
#'
#' @param Z A list with components "x", "y" and "z". Here, x and y are the 
#'          coordinates of the grid on which the data is observed and z is an 
#'          array with dimensions c(length(x),length(y),n), containing the
#'          data. n is the number of observations.
#' @param level The level of interest.
#' @param X The design matrix of the linear model. If NULL, it is set to 
#'          matrix(rep(1,dim(Y)[3]),ncol=1) corresponding to i.i.d. data.
#' @param w A vector of length nrow(X) indicating the desired linear combination
#'          of coefficients to be used in inference, i.e., t(w) %*% coeffs.  If 
#'          NULL, the default is c(1, rep(0, ncol(X) - 1)).
#' @param correlation Type of correlation assumed for the spatially indexed 
#'                    indexed linear models. This is a string that is passed to
#'                    the function gls from the nlme package. Defaults to NULL 
#'                    which corresponds to i.i.d. errors.
#' @param corpar A list of parameters passed to the correlation function.
#' @param groups A factor vector describing groups that are used in the \code{correlation} function.  Should have the same length as \code{X}.
#' @param V A 4-dimensional array containing the covariance matrix associated with each element of \code{Z$z}.  See Details.
#' @param alpha The significance level. Inclusion for CoPE sets holds with 
#'              probability 1-alpha.
#' @param N The number of bootstrap realizations to generate for determining
#'          the threshold.
#' @param mu The true parameter function. Use the default NULL if unknown. 
#' @param mask Pixels outside the mask (i.e. where mask is == NA) are ignored.
#'   
#' @return An object of class cope. A list containing the following
#' \itemize{
#'  \item{x, y: }{The grid coordinates from the input.}
#'  \item{mu, level, tau, X, N, alpha, mask: }
#'  {The corresponding values from the input.}
#'  \item{mu_hat, norm_est: }{The estimatot for mu and its normalized version.}
#'  \item{a_MB, a_MB_true, a_Tay, a_Tay_true: }{Thresholds for the CoPE sets 
#'  determined using the multiplier bootstrap or Taylor's method and the 
#'  estimated or the true contour, respectively.}
#'  \item{incl_MB, incl_MB_true, incl_Tay, incl_Tay_true: }{Booleans indicating 
#'  whether inclusion of the excursion set in the upper CoPE set and inclusion
#'  of the lower CoPE set in the excursion set holds, when CoPE sets are
#'  determined by a_MB, a_MB_true, a_Tay or a_Tay_true, respectively. Only 
#'  available if mu is given.}
#' }
#' 
#' @export
#' 
#' @references M. Sommerfeld, S. Sain and A. Schwartzman. Confidence regions for 
#'             excursion sets in asymptotically Gaussian
#'             random fields, with an application to climate. Preprint, 2015. 
#'        
#' @importFrom stats formula sd quantile
#' @importFrom grDevices contourLines
#' @examples
#' # An example using the ToyNoise and ToySignal of this package.
#' \dontrun{
#' n = 30
#' Data = ToyNoise1(n = n)
#' Data$z = Data$z + rep(ToySignal()$z, n)
#' CopeSet = ComputeCope(Data,level=4/3, mu=ToySignal()$z)
#' PlotCope(CopeSet)}
ComputeCope = function(Z, level,
                X=NULL,
                w = NULL,
                correlation = NULL,
                corpar = NULL,
                groups = NULL,
                V = NULL,
                alpha=0.1,
                N=1000,
                mu=NULL,
                mask=NULL){
  x = Z$x
  y = Z$y
  Y = Z$z
  n = dim(Y)[3]
  nloc <- length(x) * length(y)
  
  if(is.null(X)){
    X <- matrix(1, n, 1)
    w <- matrix(1, 1, 1)
  } 
  p <- ncol(X)
  
  if(is.null(groups)){
    groups <- rep(1, n)
  }
  
  
  # Compute the inverse of the square-root of a positive definite matrix.
  invsqrtm <- function(A){
    E <- eigen(A)
    U <- E$vectors
    D <- diag(E$values)
    U %*%  diag(1 / sqrt(E$values)) %*% t(U)
  }
 
  # For each location compute the GLS estimate, the de-correlated residuals and 
  # vabs and the normalized estimator.
  deR <- array(0, c(length(x), length(y), n)) # De-correlated residuals.
  vabs <- matrix(0, length(x), length(y)) # Absolute value of v.
  norm_est <- matrix(0, length(x), length(y)) # Normalized estimator.
  mu_hat <- matrix(0, length(x), length(y)) # Estimate of the function of 
                                            # interest.
  if(!is.null(correlation)) correlation = do.call(get(correlation), c(corpar, form = ~1|groups))
  
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      ytemp <- Y[i, j,]
      if (sum(is.na(ytemp)) == length(ytemp)) { # deal with problem if ytemp is all NAs
        mu_hat[i,j] = NA
        norm_est[i,j] = NA
      }else{
        df <- data.frame(cbind(ytemp = ytemp, X, groups = groups))
        df <- df[order(groups),]
        groups <- sort(groups)
        
        fo <- paste(names(df)[1], "~",
                    paste(names(df)[-c(1, p + 2)], collapse = " + "), "-1")
        if (is.null(V)) {
          model <-
            nlme::gls(formula(fo), data = df, correlation = correlation)
        }else{
          model <-
            MASS::lm.gls(formula(fo), data = df, W = V[i,j,,], inverse = TRUE)
        }
        
        #       if(is.null(correlation)){
        #        model <- nlme::gls(formula(fo), data = df)
        #       }else{
        #        model <- nlme::gls(formula(fo), data = df,
        #                           correlation = do.call(get(correlation),
        #                                                 c(corpar, form = ~1|groups)))
        #       }
        
        mu_hat[i, j] <- t(w) %*% model$coefficients
        
        if (!is.null(correlation)) {
          cM <- nlme::corMatrix(model$modelStruct$corStruct, corr = F)
          if (!is.list(cM))
            cM <- list(cM)
          invsqrtmOmega <-
            Matrix::as.matrix(Matrix::bdiag(cM))
          deR[i, j,] <- invsqrtmOmega %*% model$residuals
          deR[i, j,] <- deR[i, j,] / sd(deR[i, j,])
        } else if (!is.null(V)) {
          deR[i, j,] <- chol(solve(V[i,j,,])) %*% model$residuals
          deR[i, j,] <- deR[i, j,] / sd(deR[i, j,])
          model$varBeta = solve(t(X) %*% solve(V[i,j,,], X))
        }else{
          deR[i, j,] <- model$residuals
          deR[i, j,] <- deR[i, j,] / sd(deR[i, j,])
        }
        
        vabs[i, j] <- sqrt(t(w) %*% model$varBeta %*% w)
        
        norm_est[i, j] <- (mu_hat[i, j] - level) / vabs[i, j]
      }
    }
  }
  
  #Ignore values not on mask.
  #if(!is.null(mask)) beta_hat[,,1][is.na(mask)] = level-10
  if(is.null(mask)){
    mask = array(1, dim = c(length(x), length(y)))
  }else{
    mask[which(!is.na(mask), arr.ind = TRUE)] = 1
  }
  
  mu_hat <- mu_hat * mask
  norm_est <- norm_est * mask
  if(!is.null(mu)) mu <- mu * mask
  
  # Compute contour of estimate.
  cont <- contourLines(list(x=x,y=y,z=mu_hat),levels=level,nlevels=1)
  
  #Compute a using Multiplier Bootstrap.
  a_MB = quantile(MBContour(x=x, y=y, R=deR, N=N, cont=cont),
                  probs=1-alpha,type=8)
  #Compute a using Taylors method.
  Tay_fun = TaylorContour(x=x, y=y, cont=cont, R=deR)
  a_Tay = 0
  while(2*Tay_fun(a_Tay)>alpha) a_Tay = a_Tay + 0.01  #The two is for the absolute value.
  
  #Compute threshold a with the true boundary if available.
  if(is.null(mu)) {a_MB_true = NA; a_Tay_true = NA} else{
    cont_true <- contourLines(list(x=x,y=y,z=mu),levels=level,nlevels=1)
    #Compute a using Multiplier Bootstrap.
    a_MB_true = quantile(MBContour(x=x,y=y,R=deR,N=N,cont=cont_true),
                         probs=1-alpha,type=8)
    #Compute a using Taylors method.
    Tay_fun = TaylorContour(x=x,y=y,cont=cont_true,R=deR) #The two is for the absolute value.
    a_Tay_true = 0
    while(2*Tay_fun(a_Tay_true)>alpha) a_Tay_true = a_Tay_true + 0.01
  }
  
  
  #Determine whether inclusion holds.
  # if(is.null(mu)){incl_MB = NA; incl_Tay = NA; incl_MB_true = NA; incl_Tay_true = NA} else{
  #   incl_MB = SubsetContour(x,y,norm_est,a_MB,mu,level) & SubsetContour(x,y,mu,level,norm_est,-a_MB)
  #   incl_Tay = SubsetContour(x,y,norm_est,a_Tay,mu,level) & SubsetContour(x,y,mu,level,norm_est,-a_Tay)
  #   incl_MB_true = SubsetContour(x,y,norm_est,a_MB_true,mu,level) & SubsetContour(x,y,mu,level,norm_est,-a_MB_true)
  #   incl_Tay_true = SubsetContour(x,y,norm_est,a_Tay_true,mu,level) & SubsetContour(x,y,mu,level,norm_est,-a_Tay_true)
  # }
  
  incl <- function(A, B){
    min(B - A) >= 0
  }
  
    if(is.null(mu)){incl_MB = NA; incl_Tay = NA; incl_MB_true = NA; incl_Tay_true = NA} else{
      Ac <- mu >= level
    incl_MB <- incl(norm_est >= a_MB, Ac) & incl(Ac, norm_est >= - a_MB)
    incl_Tay <- incl(norm_est >= a_Tay, Ac) & incl(Ac, norm_est >= - a_Tay)
    incl_MB_true <- incl(norm_est >= a_MB_true, Ac) & incl(Ac, norm_est >= - a_MB_true)
    incl_Tay_true <- incl(norm_est >= a_Tay_true, Ac) & incl(Ac, norm_est >= - a_Tay_true)
  }
  
  result = structure(
    list(x=x,y=y,mu=mu,level=level,mu_hat=mu_hat,w = w,X=X,alpha=alpha,N=N,norm_est=norm_est, 
         a_MB=a_MB,a_Tay=a_Tay,a_MB_true=a_MB_true,a_Tay_true=a_Tay_true,
         incl_MB=incl_MB, incl_Tay=incl_Tay,incl_MB_true=incl_MB_true,incl_Tay_true=incl_Tay_true, mask=mask), class = "cope")
}

#' Plots CoPE sets. 
#'
#' @param cope An object of class cope to be plotted.
#' @param plot.taylor  Boolean indicating whether the CoPE sets with the threshold 
#'                      obtained by Taylor's method should be plotted. Default is
#'                      FALSE. 
#' @param use.true.function  Boolean indicating whether the threshold obtained 
#'                            from the true function should be used. Default is 
#'                            FALSE.  
#' @param map If TRUE plot the cope set on a map of the world. The coordinates 
#'            in this case are interpreted as longitude and latitude.  
#' @param ... Additional graphical parameters passed to fields::image.plot.
#' @export
PlotCope = function(cope,plot.taylor=FALSE, use.true.function = FALSE, map=FALSE, ...){
  
  if(map){
    plot.function = ImageMap
  } else{
    plot.function = fields::image.plot
  }
  
  if(use.true.function & is.null(cope$mu)){
    print("True function is not available. Estimated boundary will be used.")
  } 
  
  x = cope$x
  y = cope$y
  
  if(use.true.function & !is.null(cope$mu)){
    if(plot.taylor) a = cope$a_Tay_true else a = cope$a_MB_true
    plot.function(x,y,cope$mu_hat,horizontal=FALSE, mask=cope$mask, ...)
    DrawContour(x,y,cope$mu,level=cope$level,col="purple", lty = 2)
    DrawContour(x,y,cope$norm_est,level=a,col="darkred")
    DrawContour(x,y,cope$norm_est,level=-a,col="darkgreen")
  } else{
    if(plot.taylor) a = cope$a_Tay else a = cope$a_MB
    plot.function(x,y,cope$mu_hat,horizontal=FALSE, mask=cope$mask, ...)
    if(!is.null(cope$mu)){
      DrawContour(x,y,cope$mu,level=cope$level,col="purple", lty = 2)
      DrawContour(x,y,cope$mu_hat,level=cope$level,col="purple", lty = 1)
    } else{
      DrawContour(x,y,cope$mu_hat,level=cope$level,col="purple", lty = 1)
    }
    DrawContour(x,y,cope$norm_est,level=a,col="darkred")
    DrawContour(x,y,cope$norm_est,level=-a,col="darkgreen")
  }
}

#' Plots CoPE sets. 
#'
#' @param x An object of class cope to be plotted.
#' @param ... Additional graphical parameters passed to fields::image.plot.
#' @param taylor  Boolean indicating whether the CoPE sets with the threshold 
#'                      obtained by Taylor's method should be plotted. Default is
#'                      FALSE. 
#' @param use.true.function  Boolean indicating whether the threshold obtained 
#'                            from the true function should be used. Default is 
#'                            FALSE.  
#' @param colc Color of contour line for \eqn{A_c}.
#' @param lwdc Width of contour line for \eqn{A_c}.
#' @param ltyc Type of contour line for \eqn{A_c}.
#' @param colp Color of contour line for \eqn{\hat{A}^{+}_c}.
#' @param lwdp Width of contour line for \eqn{\hat{A}^{+}_c}.
#' @param ltyp Type of contour line for \eqn{\hat{A}^{+}_c}.
#' @param colm Color of contour line for \eqn{\hat{A}^{-}_c}.
#' @param lwdm Width of contour line for \eqn{\hat{A}^{-}_c}.
#' @param ltym Type of contour line for \eqn{\hat{A}^{-}_c}.
#' @param conlist A list of additional arguments to pass to the \code{contour} function.
#'                By default, the contour labels are not shown.
#' @importFrom fields image.plot
#' @importFrom graphics contour
#' @method plot cope
#' @export
#' @references M. Sommerfeld, S. Sain and A. Schwartzman. Confidence regions for 
#'             excursion sets in asymptotically Gaussian
#'             random fields, with an application to climate. Preprint, 2015. 
#' @examples
#' # An example using the ToyNoise and ToySignal of this package.
#' \dontrun{
#' n = 30
#' Data = ToyNoise1(n = n)
#' Data$z = Data$z + rep(ToySignal()$z, n)
#' CopeSet = ComputeCope(Data, level=4/3, mu=ToySignal()$z)
#' plot(CopeSet)}

plot.cope = function(x, ..., taylor = FALSE, use.true.function = FALSE,
                     colc = "purple", lwdc = 3, ltyc = 1,
                     colp = "darkred", lwdp = 3, ltyp = 1,
                     colm = "darkgreen", lwdm = 3, ltym = 1,
                     conlist = list(drawlabels = FALSE))
{
  if(use.true.function & is.null(x$mu)){
    stop("True function is not available. Estimated boundary will be used.")
  }
  
  # determine whether to use the true contour for Ac.  Otherwise, 
  # use estimate

  if(use.true.function){
    if(taylor) a = x$a_Tay_true else a = x$a_MB_true
    Acz = x$mask*x$mu
    # fields::image.plot(x, y, x$mask*x$mu_hat, ...)
    # DrawContour(x,y,x$mu,level=x$level,col="purple")
    # DrawContour(x,y,x$norm_est,level=a,col="darkred")
    # DrawContour(x,y,x$norm_est,level=-a,col="darkgreen")
  } else{
    if(taylor) a = x$a_Tay else a = x$a_MB
    #fields::image.plot(x, y, x$mask*x$mu_hat, ...)
    Acz = x$mask*x$mu_hat
#     if(!is.null(x$mu)){
#       DrawContour(x,y,x$mu,level=x$level,col="purple")
#     } else{
#       DrawContour(x,y,x$mu_hat,level=x$level,col="purple")
#     }
#     DrawContour(x,y,x$norm_est,level=a,col="darkred")
#     DrawContour(x,y,x$norm_est,level=-a,col="darkgreen")
  }
#   
#   
#     
#   if(use.true.function)
#   {
#     Acz = x$mask*x$mu 
#     if(taylor) a = x$a_Tay_true else a = x$a_MB_true
#   }else
#   {
#     Acz = x$mask*x$mu_hat
#     if(taylor) a = x$a_Tay else a = x$a_MB
#   }
  
  # collect arguments for various contours (Ac, Ac+, Ac-)
  cAcz = Acz
  if(!is.null(x$mu)) cAcz = x$mu # determine whether true or 
                                 # estimated contour should be used
  argc = c(list(x = x$x, y = x$y, z = cAcz, level = x$level,
              col = colc, lwd = lwdc, lty = ltyc, add = TRUE),
              conlist)
  argp = c(list(x = x$x, y = x$y, z = x$norm_est, level = a,
                col = colp, lwd = lwdp, lty = ltyp, add = TRUE),
                conlist)
  argm = c(list(x = x$x, y = x$y, z = x$norm_est, level = -a,
                col = colm, lwd = lwdm, lty = ltym, add = TRUE),
                conlist)
  
  # plot image plot of Acz, and relevant contours
  fields::image.plot(x = x$x, y = x$y, Acz, ...)
  do.call(contour, argc)
  do.call(contour, argp)
  do.call(contour, argm)  
}
