#' Compute p-values from information gains and return MDFS
#'
#' @param IG max conditional information gains
#' @param dimensions number of dimensions
#' @param divisions number of divisions
#' @param response.divisions number of response divisions (i.e. categories-1)
#' @param df vector of degrees of freedom for each variable (optional)
#' @param contrast.mask boolean mask on \code{IG} specifying which variables are contrast variables (or \code{NULL} if none, otherwise at least 3 variables must be marked)
#' @param ig.in.bits \code{TRUE} if input is in binary log (as opposed to natural log)
#' @param ig.doubled \code{TRUE} if input is doubled (to follow the chi-squared distribution)
#' @param one.dim.mode \code{'exp'} for exponential distribution, \code{'lin'} for linear function of chi-squared or \code{'raw'} for raw chi-squared
#' @param irr.vars.num if not NULL, number of irrelevant variables, specified by the user
#' @param ign.low.ig.vars.num if not NULL, number of ignored low IG variables, specified by the user
#' @param min.irr.vars.num minimum number of irrelevant variables (\code{NULL} selects probable optimal number)
#' @param max.ign.low.ig.vars.num maximum number of ignored low IG variables (\code{NULL} selects probable optimal number)
#' @param search.points number of points in search procedure for the optimal number of ignored variables
#' @param level acceptable error level of goodness-of-fit one-sample Kolmogorov-Smirnov test (used only for warning)
#' @return A \code{\link{data.frame}} with class set to \code{MDFS}. Can be coerced back to \code{data.frame} using \code{\link{as.data.frame}}.
#'
#'  The following columns are present:
#'  \itemize{
#'    \item \code{IG} -- information gains (input copy)
#'    \item \code{chi.squared.p.value} -- chi-squared p-values
#'    \item \code{p.value} -- theoretical p-values
#'  }
#'
#'  Additionally the following \code{\link{attributes}} are set:
#'  \itemize{
#'   \item \code{run.params} -- run parameters
#'   \item \code{sq.dev} -- vector of square deviations used to estimate the number of irrelevant variables
#'   \item \code{dist.param} -- distribution parameter
#'   \item \code{err.param} -- squared error of the distribution parameter
#'   \item \code{fit.p.value} -- p-value of fit
#'  }
#' @examples
#' ComputePValue(madelon$IG.2D, dimensions = 2, divisions = 1)
#' @importFrom stats pchisq ks.test
#' @export
ComputePValue <- function(
  IG,
  dimensions,
  divisions,
  response.divisions = 1,
  df = NULL,
  contrast.mask = NULL,
  ig.in.bits = TRUE,
  ig.doubled = FALSE,
  one.dim.mode = 'exp',
  irr.vars.num = NULL,
  ign.low.ig.vars.num = NULL,
  min.irr.vars.num = NULL,
  max.ign.low.ig.vars.num = NULL,
  search.points = 8,
  level = 0.05
) {
  #check the reasonability of input
  ll<-ifelse(is.null(contrast.mask), length(IG), length(IG[contrast.mask]))

  if (length(IG)<4 || sum(is.na(as.numeric(IG)))>0 || sum(!is.numeric(IG))>0) {
   stop('IG has to be a numeric vector of length above 3, without N/A values')
  }

  if (as.integer(dimensions) != dimensions || dimensions < 1) {
   stop('Dimensions has to be a positive integer')
  }

  if (as.integer(divisions) != divisions || divisions < 1) {
   stop('Divisions has to be a positive integer')
  }

  if (as.integer(response.divisions) != response.divisions || response.divisions < 1) {
   stop('Response.divisions has to be a positive integer')
  }

  if (!is.null(df)) {
   if (sum(is.na(as.integer(df)))>0 || sum(as.integer(df)!=df)>0 || sum(df<1)>0) stop('Df has to be a vector of positive integers')
   if (length(df) != 1 && length(df) != length(IG)) stop('Df has to have the same length as IG or 1')
  }

  if (!is.null(contrast.mask) && ll<4) {
    stop('Contrast.mask (if not NULL) has to specify a vector of length above 3')
  }

  if (dimensions==1 && one.dim.mode!="exp" && one.dim.mode!="lin" && one.dim.mode!="raw") {
   stop('One.dim.mode has to be \'raw\', \'lin\' or \'exp\'')
  } # one.dim.mode does not matter, when dimensions>1

  if (is.null(min.irr.vars.num)) {
    min.irr.vars.num <- min(ifelse(is.null(contrast.mask), length(IG), length(IG[contrast.mask]))%/%3, 30)
  } else if (as.integer(min.irr.vars.num) != min.irr.vars.num || min.irr.vars.num<3) {
    stop('Min.irr.vars.num has to be an integer bigger than 2')
  }

  if (is.null(max.ign.low.ig.vars.num)) {
    max.ign.low.ig.vars.num <- ifelse(is.null(contrast.mask),
                                      min(length(IG)-min.irr.vars.num, length(IG)%/%3),
                                      min(length(IG[contrast.mask])-min.irr.vars.num, length(IG[contrast.mask])%/%3))
  } else if (as.integer(max.ign.low.ig.vars.num) != max.ign.low.ig.vars.num || max.ign.low.ig.vars.num<0) {
    stop('Max.ign.low.ig.vars.num has to be a non-negative integer')
  }

  if (max.ign.low.ig.vars.num+min.irr.vars.num>ll) {
    stop('Sum of min.ign.low.ig.vars.num and max.irr.vars.num has to be lower than the IG vector length')
  }

  if (!is.null(irr.vars.num) && (as.integer(irr.vars.num) != irr.vars.num || irr.vars.num<3)) {
   stop('Irr. vars. num has to be an integer bigger than 2')
  }

  if (!is.null(irr.vars.num) && irr.vars.num<min.irr.vars.num) {
   stop('Irr.vars.num has to be bigger than min.irr.vars.num')
  }

  if (!is.null(ign.low.ig.vars.num) && (as.integer(ign.low.ig.vars.num) != ign.low.ig.vars.num || ign.low.ig.vars.num<0)) {
   stop('Ign. low. ig. vars. num has to be a non-negative integer')
  }

  if (!is.null(ign.low.ig.vars.num) && ign.low.ig.vars.num>max.ign.low.ig.vars.num) {
   stop('Ign.low.ig.vars.num has to be smaller than max.ign.low.ig.vars.num')
  }

  if (ifelse(!is.null(ign.low.ig.vars.num), ign.low.ig.vars.num, max.ign.low.ig.vars.num)+ifelse(!is.null(irr.vars.num), irr.vars.num, min.irr.vars.num)>ll) {
   stop('Sum of ign.low.ig.vars.num and irr.vars.num has to be lower than the IG vector length')
  }

  if (!is.null(ign.low.ig.vars.num) && (as.integer(search.points) != search.points || search.points<2)) {
   stop('Search.points has to be an integer bigger than 1')
  }

  #bits to nats and the factor 2
  if (ig.in.bits) IG<-log(2)*IG
  if (!ig.doubled) IG<-2*IG

  #IG must be positive
  IG.original<-IG
  IG[IG<1e-8]<-1e-8

  #order variables
  order.IG<-order(IG.original)

  #degrees of freedom - if not specified by the user
  if (is.null(df)) df<-response.divisions*divisions*(divisions+1)^(dimensions-1)

  #compute p-values
  chisq<-pchisq(IG,df,lower.tail=FALSE)
  chisq.log<-pchisq(IG,df,log.p=TRUE)

  #if there are contrast variables, use them to compute the parameter; otherwise use all the variables
  if (!is.null(contrast.mask)) {
    IG.contrast<-IG[contrast.mask]
    order.IG.contrast<-order(IG.contrast)
    chisq.contrast<-(chisq[contrast.mask])[order.IG.contrast]
  } else {
    IG.contrast<-IG
    order.IG.contrast<-order.IG
    chisq.contrast<-chisq[order.IG]
  }

  n.var<-length(IG.contrast)

  #weights and index
  K<-1:n.var
  V<-chisq.contrast[K]
  if (dimensions>1 || one.dim.mode=="exp") {
    L<-log(K)
    W<-K/(n.var-K+1)
  } else {
    L<-K
    W<-1/K/(n.var-K+1)
  }
  #cumulative sums overall
  Sw<-cumsum(W)
  Sv<-cumsum(V)
  Swv<-cumsum(W*V)
  Swv2<-cumsum(W*V^2)
  Swl<-cumsum(W*L)
  Swl2<-cumsum(W*L^2)
  Swlv<-cumsum(W*L*V)
  Srt<-cumsum(W[n.var:1]*(chisq.contrast[n.var:1])^2)[n.var:1]

  #fit the distribution parameter
  calc.S<-function(n0i) {
    npt<-n.var-n0i+1
    k<-K[1:npt]
    l<-L[1:npt]
    sw<-Sw[n0i:n.var]-c(0,Sw)[n0i]
    sv<-Sv[n0i:n.var]-c(0,Sv)[n0i]
    swv<-Swv[n0i:n.var]-c(0,Swv)[n0i]
    swv2<-Swv2[n0i:n.var]-c(0,Swv2)[n0i]
    swl<-Swl[n0i:n.var]-c(0,Swl)[n0i]
    swl2<-Swl2[n0i:n.var]-c(0,Swl2)[n0i]
    swlv<-Swlv[n0i:n.var]-c(0,Swlv)[n0i]
    srt<-Srt[n0i:n.var]

    if (dimensions>1 || one.dim.mode=="exp") {
      alpha<-sv/k
      S<-(swv2+2*alpha*swlv-2*alpha*l*swv+alpha^2*swl2-2*alpha^2*l*swl+(alpha*l)^2*sw)
      d.alpha<-S/(swl2-2*l*swl+l^2*sw)
    } else {
      if (one.dim.mode=="raw") alpha<-rep(1,npt) else alpha<-(swv-swlv/l)/(sw-2*swl/l+swl2/l^2)
      S<-(swv2-2*alpha*swv+2*alpha/l*swlv+alpha^2*sw-2*alpha^2/l*swl+alpha^2/l^2*swl2)
      d.alpha<-S/(sw-2*swl/l+swl2/l^2)
    }
    S<-(c(0,S)+c(srt,0))[1:npt]/sw[npt]
    return(cbind(S,alpha,d.alpha))
  }

  #search for the optimal number of ignored variables
  if (is.null(ign.low.ig.vars.num)) {
   n0min<-1
   n0max<-max.ign.low.ig.vars.num
   Smin<-+Inf
   n0<-nv<--1
   step<-+Inf
   while (step>1) {
    step<-max(1,round((n0max-n0min)/search.points))
    n0i<-n0min
    while (n0i<n0max) {

     params<-calc.S(n0i)
     S<-params[,1]
     nvi<-ifelse(!is.null(irr.vars.num), irr.vars.num,                              #irr.vars.num specified by the user
                                         ifelse(!is.null(contrast.mask), length(S), #contrast variables are all irrelevant
                                                                         min.irr.vars.num-1+which.min(S[min.irr.vars.num:length(S)])))
     Si<-S[nvi]

     if (Si<Smin) {
      n0<-n0i
      nv<-nvi
      Smin<-Si

      alpha<-params[nv,2]
      d.alpha<-sqrt(abs(params[nv,3]))
      sq.dev<-S
     }
     n0i<-n0i+step
    }
    if (n0>0) {
     n0min<-max(1,n0-step)
     n0max<-min(max.ign.low.ig.vars.num,n0+step)
    }
   }
  } else {          #ign.low.ig.vars.num specified by the user
   n0<-ign.low.ig.vars.num+1
   params<-calc.S(n0)
   S<-params[,1]
   nv<-ifelse(!is.null(irr.vars.num), irr.vars.num,                              #irr.vars.num specified by the user
                                      ifelse(!is.null(contrast.mask), length(S), #contrast variables are all irrelevant
                                                                      min.irr.vars.num-1+which.min(S[min.irr.vars.num:length(S)])))
   alpha<-params[nv,2]
   d.alpha<-sqrt(abs(params[nv,3]))
   sq.dev<-params[,1]
  }

  ign.low.ig.vars.num<-min(n0-1,max.ign.low.ig.vars.num)
  irr.vars.num<-max(nv,min.irr.vars.num)

  if (ign.low.ig.vars.num>=max.ign.low.ig.vars.num) {warning("Border value reached for ignored variable number")}
  if (irr.vars.num<=min.irr.vars.num) {warning("Border value reached for irrelevant variable number")}

  if (dimensions>1 || one.dim.mode=="exp") {
   p.values<- -expm1(chisq.log/alpha)
  } else {
   p.values<-chisq/alpha
  }

  #test for goodness of fit
  if (!is.null(contrast.mask)) { p.test<-p.values[contrast.mask] } else p.test<-p.values
  p.test<-rev(p.test[order(p.test,decreasing=T)][n0:(n0+nv)])
  pv.fit<-suppressWarnings(ks.test(p.test,'punif'))$p.value

  result <- data.frame(
    IG = IG.original,
    chi.squared.p.value = chisq,
    p.value = p.values
  )
  class(result) <- 'MDFS'

  attr(result, 'run.params') <- list(
    contrast.mask           = contrast.mask,
    dimensions              = dimensions,
    divisions               = divisions,
    response.divisions      = response.divisions,
    df                      = df,
    search.points           = search.points,
    ign.low.ig.vars.num     = ign.low.ig.vars.num,
    max.ign.low.ig.vars.num = max.ign.low.ig.vars.num,
    irr.vars.num            = irr.vars.num,
    min.irr.vars.num        = min.irr.vars.num,
    one.dim.mode            = one.dim.mode,
    ig.in.bits              = ig.in.bits,
    ig.doubled              = ig.doubled)

  attr(result, 'sq.dev') <- sq.dev
  attr(result, 'dist.param') <- alpha
  attr(result, 'err.param') <- d.alpha
  attr(result, 'fit.p.value') <-pv.fit

  return(result)
}


#' Find indices of relevant variables
#'
#' @param fs feature selector
#' @param ... arguments passed to methods
#' @return indices of important variables
#' @export
RelevantVariables <- function(fs, ...) {
  UseMethod('RelevantVariables')
}

#' Find indices of relevant variables from MDFS
#'
#' @details
#' In case of FDR control it is recommended to use Benjamini-Hochberg-Yekutieli p-value adjustment
#' method (\code{"BY"} in \code{\link[stats]{p.adjust}}) due to unknown dependencies between tests.
#'
#' @param fs an MDFS object
#' @param level statistical significance level
#' @param p.adjust.method method as accepted by \code{\link[stats]{p.adjust}} (\code{"BY"} is recommended for FDR, see Details)
#' @param ... ignored
#' @return indices of relevant variables
#' @importFrom stats p.adjust
#' @export
RelevantVariables.MDFS <- function(fs, level=0.05, p.adjust.method="holm", ...) {
  contrast.mask <- attr(fs, 'run.params')$contrast.mask
  p.value <- if(is.null(contrast.mask)) { fs$p.value } else { fs$p.value[!contrast.mask] }
  return(which(p.adjust(p.value, method=p.adjust.method)<level))
}

#' Plot MDFS details
#'
#' @param x an MDFS object
#' @param plots plots to plot (ig for max IG, c for chi-squared p-values, p for p-values)
#' @param ... passed on to \code{\link[graphics]{plot}}
#' @return No return value, called for side effects.
#' @importFrom graphics plot
#' @export
plot.MDFS <- function(x, plots=c('ig', 'c', 'p'), ...) {
  ord <- order(x$IG,decreasing=TRUE)
  for (plt in plots) {
    switch(plt,
           ig = plot(x$IG[ord], xlab='index', ylab=expression('I'[max]*'(X)'), ...),
           c = plot(x$chi.squared.p.value[ord], seq(ord)/length(ord), xlab='chi-squared p-value', ylab='experimental p-value', ...),
           p = plot(x$p.value[ord], seq(ord)/length(ord), xlab='theoretical p-value', ylab='experimental p-value', ...),
           stop(paste('I don\'t know how to plot', plt)))
  }
}

#' as.data.frame S3 method implementation for MDFS
#'
#' @param x an MDFS object
#' @param ... ignored
#' @return data.frame
#' @export
as.data.frame.MDFS <- function(x, ...) {
  class(x) <- 'data.frame'
  return(x)
}
