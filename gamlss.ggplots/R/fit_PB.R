################################################################################
################################################################################
################################################################################
################################################################################
#  Function 1 fit_PB
fit_PB <- function(x, y, weights, data,  xmin, xmax, nseg = 20,
                   lambda = 10, order=2, degree=3, max.df=20, ylim, 
                   plot=TRUE, col.ribbon="pink")
{
################################################################################
  bbase <- function(x, xl, xr, ndx, deg)
  {
    # Truncated p-th power function  
    tpower <- function(x, t, p) (x - t) ^ p * (x > t)
    dx <- (xr - xl) / ndx # DS increment 
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    P <- outer(x, knots, tpower, deg)# calculate the power in the knots
    n <- dim(P)[2]
    D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg) # 
    B <- (-1) ^ (deg + 1) * P %*% t(D)
    attr(B, "knots") <- knots
    attr(B, "rest") <- c(xmin=xl, xmax=xr, nknots=ndx, degree=deg)
    B 
  }
################################################################################ 
  lower <- upper <- knots <- NULL
  scall <- deparse(sys.call())
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))
  y <- if (!missing(data)) 
    get(deparse(substitute(y)), envir = as.environment(data))
  else y
  x <- if (!missing(data)) 
    get(deparse(substitute(x)), envir = as.environment(data))
  else x
  w <- if (!missing(data)) 
    if (missing(weights)) w <- rep(1, length(x))
  else w <- get(deparse(substitute(weights)), envir = as.environment(data))
  else 
  {
    if (missing(weights)) w <- rep(1, length(x))
    else weights
  }
  if (is.factor(x)||is.character(x)) stop("x should be continuous")   
  if (length(w)!=length(y)) stop("response and weights should be the same length")
  xmin <- if (missing(xmin))  min(x) else xmin
  xmax <- if (missing(xmax))  max(x) else xmax
  #     x1 <- seq(min(x), max(x), length=500)
  B <- bbase(x,  xl = min(x), xr = max(x), ndx=nseg, deg=degree)
  nb <- ncol(B)# may taake off 
  D <- diff(diag(nb), diff = order)
  p <- ncol(D)
  N <- sum(w!=0) # DS+FDB 3-2-14
  n <- nrow(B) # the no of observations
  qrX <- qr(sqrt(w)*B, tol=.Machine$double.eps^.8)  
  R <- qr.R(qrX)
  Q <- qr.Q(qrX) 
  Qy <- t(Q)%*%(sqrt(w)*y)
  tau2 <- sig2 <- NULL
  if (lambda>=1e+07) lambda <- 1e+07 # MS 19-4-12
  if (lambda<=1e-07) lambda <- 1e-07 # MS 19-4-12
################################################################################
  regpen <- function(y, x, w, lambda, D)# original
  {
    RD <- rbind(R,sqrt(lambda)*D) # 2p x p matrix 
    svdRD <- svd(RD)                 # U 2pxp D pxp V pxp
    ##                                        take only the important values    
    rank <- sum(svdRD$d>max(svdRD$d)*.Machine$double.eps^.8)
    U1 <- svdRD$u[1:p,1:rank]     # U1 p x rank 
    y1 <- t(U1)%*%Qy              # rankxp pxn nx1 => rank x 1 vector 
    beta <- svdRD$v[,1:rank] %*%(y1/svdRD$d[1:rank])
    HH <- (svdRD$u)[1:p,1:rank]%*%t(svdRD$u[1:p,1:rank])
    df <- sum(diag(HH))
    Vcov <-  svdRD$v%*%diag(1/svdRD$d^2)%*%t(svdRD$v)# added 21-3-2021
    fit <- list(beta = beta, edf = df, lev=diag(HH), vcov=Vcov)
    return(fit)  
  }
################################################################################
  fit <- regpen(y, B, w, lambda,  D)
  # REML  
  for (it in 1:50) 
  {     fv <- B %*% fit$beta  
  fit <- regpen(y, B, w, lambda, D) # fit model
  gamma. <- D %*% as.vector(fit$beta)  # get the gamma differences
  fv <- B %*% fit$beta             # fitted values
  sig2 <- sum(w * (y - fv) ^ 2) / (N - fit$edf) # DS+FDB 3-2-14
  tau2 <- sum(gamma. ^ 2) / (fit$edf-order)# see LNP page 279
  if(tau2<1e-7) tau2 <- 1.0e-7 # MS 19-4-12
  lambda.old <- lambda
  lambda <- sig2 / tau2 # maybe only 1/tau2 will do since it gives exactly the EM results see LM-1
  if (lambda<1.0e-7) lambda<-1.0e-7 # DS Saturday, April 11, 2009 at 14:18
  if (lambda>1.0e+7) lambda<-1.0e+7 # DS 29 3 2012
  if (abs(lambda-lambda.old) < 1.0e-7||lambda>1.0e10) break
  }
  if (fit$edf>=max.df) 
  {
#local function to get max.df###################################################
    edf2_df <- function(loglambda)
    {
      lambda <- exp(loglambda)
      I.lambda.D <- (1+lambda*UDU$values)
      edf <- sum(1/I.lambda.D)
      (edf-max.df)
    }  
################################################################################
    Rinv <- try(solve(R), silent = TRUE)
    if (any(class(Rinv) %in% "try-error"))
      stop("The B-basis for ",xname," is singular, transforming the variable may help","\n")
    S <- t(D)%*%D
    UDU <- eigen(t(Rinv)%*%S%*%Rinv, symmetric=TRUE, only.values=TRUE) 
    loglambda <- if (sign(edf2_df(-30))==sign(edf2_df(30))) 30  
    else   uniroot(edf2_df, c(-30,30))$root          
    lambda <- exp(loglambda)
    fit <- regpen(y, x, w, lambda, D)
    if (abs(fit$edf-max.df)>0.1) warning("the target df's are not acheived, try to reduce the no. of knot intervals \n in pb(). eg. nseq=10")
    fv <- B %*% fit$beta 
  } 
################################################################################
  waug <- as.vector(c(w, rep(1,nrow(D))))
  xaug <- as.matrix(rbind(B,sqrt(lambda)*D))
  lev <- hat(sqrt(waug)*xaug,intercept=FALSE)[1:n] # get the hat matrix
  var <- (lev/w)*sig2 
  # different way is to calculate the vcoc of gamma   
  vg  <-  fit$vcov*sig2
  # this can be done also using PB equation 2.16   
  # vgamma <- solve(t(B)%*%diag(w)%*%B+lambda*t(D)%*%D)*sig2
  varfv <- B%*%vg%*%t(B) # variance covariance for fitted values n*n
  # var and diag(varfv) should be identical
  # plot(diag(varfv),var); lines(var, var)
  if (plot)
  {
    
    df <- data.frame(y=y, x=x, fv=fv, lower=fv-2*sqrt(var), upper=fv+2*sqrt(var)) 
    #   theme_bw(base_size = 18)
    gg <- ggplot2::ggplot(data=df, ggplot2::aes(x=x, y=y))+
      ggplot2::geom_point(, col="black", size=0.5)+
      ggplot2:: geom_line( aes(x=x, y=fv), col="blue", size=1)+
      ggplot2::ylab(yname)+ xlab(xname)+
      ggplot2::geom_ribbon(data=df, aes(ymin = lower, ymax = upper, x = x), alpha=.5, fill=col.ribbon)
    print(gg)
  }
  x1 <- seq(min(x), max(x), length=500)
  rest <- attr(B, "rest")
  B1 <- bbase(x=x1,  xl = rest[1], xr = rest[2],  ndx = rest[3], deg = degree)
  knots <- attr(B1,"knots")
  z <- B1 %*% fit$beta   
  suppressWarnings(
    Fun <- splinefun(x1, z, method="natural"))
  s1 <- sqrt(sig2)
  deviance <- sum(-2*dNO(y, mu=fv, sigma=s1, log=TRUE))
  devianceIncr <- -2*dNO(y, mu=fv, sigma=s1, log=TRUE)
  G <- t(D)%*%D
  pen <- lambda* t(fit$beta)%*%G%*%fit$beta 
  M <- list(call = scall, y = y, x = x, fv = fv, coef = fit$beta, 
            lambda = lambda, sig = s1, sigb = sqrt(tau2), fun = Fun,  
            B = B,  deviance = deviance, df = fit$edf, vcov.coef = vg,
            varfv = var, 
            xname = xname, yname = yname, penalty = pen,  knots = knots,
            devianceIncr = devianceIncr)
  class(M) <- "Psplines"
  invisible(M)
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# methods for Psplines
print.Psplines <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{   
  cat("P-spline fit of", x$yname, "against", x$xname, "using fit_PB() \n")
  cat("Degrees of Freedom for the fit :", x$df, "\n")
  # cat("Random effect parameter sigma_b:", format(signif(x$sigb)), "\n")  
  cat("Smoothing parameter lambda     :", format(signif(x$lambda)), "\n") 
}
################################################################################
################################################################################
################################################################################
################################################################################
fitted.Psplines <- function(object,...)
{
  as.vector(object$fv)  
}
################################################################################
################################################################################
################################################################################
################################################################################

deviance.Psplines <- function(object, ...)
{
  as.vector(object$deviance)  
}
################################################################################
################################################################################
################################################################################
################################################################################
predict.Psplines <- function(object, newdata=NULL,...)
{
  pred <-  if (is.null(newdata)) object$fv
  else    object$fun(newdata)
  pred
}
################################################################################
################################################################################
################################################################################
################################################################################
coef.Psplines <- function(object,...)
{
  as.vector(object$coef)
}
################################################################################
################################################################################
################################################################################
################################################################################
residuals.Psplines  <- function(object,...)
{
  as.vector(object$y - object$fv)
}
################################################################################
################################################################################
################################################################################
################################################################################
# P Splines ends here 