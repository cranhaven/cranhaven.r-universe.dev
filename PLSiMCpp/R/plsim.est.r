

#' @name plsim.est
#' @aliases plsim.est
#' @aliases plsim.est.formula
#' @aliases plsim.est.default
#'
#' @title Profile Least Squares Estimator
#' @description PLS was proposed by Liang \emph{et al.} (2010) 
#' to estimate parameters in PLSiM
#' \deqn{Y = \eta(Z^T\alpha) + X^T\beta + \epsilon.}
#' 
#' @usage plsim.est(\dots)
#' 
#' \method{plsim.est}{formula}(formula, data, \dots)
#' 
#' \method{plsim.est}{default}(xdat=NULL, zdat, ydat, h=NULL, zetaini=NULL, MaxStep = 200L,
#' ParmaSelMethod="SimpleValidation", TestRatio=0.1, K = 3, seed=0, verbose=TRUE, \dots)
#' 
#' @param formula a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param xdat input matrix (linear covariates). The model reduces to a single index model when \code{x} is NULL.
#' @param zdat input matrix (nonlinear covariates). \code{z} should not be NULL.
#' @param ydat input vector (response variable).
#' @param h a value or a vector for bandwidth. If \code{h} is NULL, a default vector c(0.01,0.02,0.05,0.1,0.5)
#' will be set for it. \link{plsim.bw} is employed to select the optimal bandwidth when h is a vector or NULL.
#' @param zetaini initial coefficients, optional (default: NULL). It could be obtained by the function \code{\link{plsim.ini}}.
#' \code{zetaini[1:ncol(z)]} is the initial coefficient vector \eqn{\alpha_0},
#' and \code{zetaini[(ncol(z)+1):(ncol(z)+ncol(x))]} is the initial coefficient vector \eqn{\beta_0}.
#' @param MaxStep the maximum iterations, optional (default=200).
#' @param ParmaSelMethod the parameter for the function \link{plsim.bw}.
#' @param TestRatio the parameter for the function \link{plsim.bw}.
#' @param K the parameter for the function \link{plsim.bw}.
#' @param seed int, default: 0.
#' @param verbose bool, default: TRUE. Enable verbose output.
#' @param \dots additional arguments.
#'
#' @return
#' \item{eta}{estimated non-parametric part \eqn{\hat{\eta}(Z^T{\hat{\alpha} })}.}
#' \item{zeta}{estimated coefficients. \code{zeta[1:ncol(z)]} is \eqn{\hat{\alpha}}, 
#' and \code{zeta[(ncol(z)+1):(ncol(z)+ncol(x))]} is \eqn{\hat{\beta}}.}
#' \item{y_hat}{ \code{y}'s estimates.}
#' \item{mse}{mean squared errors between y and \code{y_hat}.}
#' \item{data}{data information including \code{x}, \code{z}, \code{y}, bandwidth \code{h}, 
#' initial coefficients \code{zetaini}, iteration step \code{MaxStep} and flag \code{SiMflag}. 
#' \code{SiMflag} is TRUE when \code{x} is NULL, otherwise \code{SiMflag} is FALSE.}
#' \item{Z_alpha}{\eqn{Z^T{\hat{\alpha}}}.}
#' \item{r_square}{multiple correlation coefficient.}
#' \item{variance}{variance of \code{y_hat}.}
#' \item{stdzeta}{standard error of \code{zeta}.}
#'
#' @export
#'
#' @examples
#' 
#' # EXAMPLE 1 (INTERFACE=FORMULA)
#' # To estimate parameters of partially linear single-index model (PLSiM). 
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
#'
#' beta = matrix(4,1,1)
#' 
#' # Case 1: Matrix Input
#' x = matrix(1,n,1)
#' z = matrix(runif(n*2),n,2)
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#'
#' fit = plsim.est(y~x|z)
#' summary(fit)
#' 
#' # Case 2: Vector Input
#' x = rep(1,n)
#' z1 = runif(n)
#' z2 = runif(n) 
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' fit = plsim.est(y~x|z1+z2)
#' summary(fit)
#' print(fit)
#' 
#' # EXAMPLE 2 (INTERFACE=DATA FRAME) 
#' # To estimate parameters of partially linear single-index model (PLSiM).  
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
#'
#' beta = matrix(4,1,1)
#' 
#' x = rep(1,n)
#' z1 = runif(n)
#' z2 = runif(n) 
#' X = data.frame(x)
#' Z = data.frame(z1,z2)
#' 
#' x = data.matrix(X)
#' z = data.matrix(Z)
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' fit = plsim.est(xdat=X,zdat=Z,ydat=y)
#' summary(fit)
#' print(fit)
#' 
#' @references
#'
#' H. Liang, X. Liu, R. Li, C. L. Tsai. \emph{Estimation and testing for partially linear single-index models}.
#' Annals of statistics, 2010, 38(6): 3811.


plsim.est = function(...)
{
    UseMethod("plsim.est")
}

plsim.est.formula = function(formula,data,...)
{
  mf = match.call(expand.dots = FALSE)   
  m = match(c("formula","data"),
            names(mf), nomatch = 0) 
  mf = mf[c(1,m)]
  mf.xf = mf
  
  mf[[1]] = as.name("model.frame")
  mf.xf[[1]] = as.name("model.frame")
  
  chromoly = deal_formula(mf[["formula"]])
  
  if (length(chromoly) != 3)
    stop("Invoked with improper formula, please see plsim.est documentation for proper use")
  
  bronze = lapply(chromoly, paste, collapse = " + ")
  
  mf.xf[["formula"]] = as.formula(paste(" ~ ", bronze[[2]]),
                                  env = environment(formula))
  
  mf[["formula"]] = as.formula(paste(bronze[[1]]," ~ ", bronze[[3]]),
                               env = environment(formula))
  
  
  formula.all = terms(as.formula(paste(" ~ ",bronze[[1]]," + ",bronze[[2]], " + ",bronze[[3]]),
                                 env = environment(formula)))
  
  orig.class = if (missing(data))
    sapply(eval(attr(formula.all, "variables"), environment(formula.all)),class)
  else sapply(eval(attr(formula.all, "variables"), data, environment(formula.all)),class)
  
  arguments.mfx = chromoly[[2]]
  arguments.mf = c(chromoly[[1]],chromoly[[3]])
  
  mf[["formula"]] = terms(mf[["formula"]])
  mf.xf[["formula"]] = terms(mf.xf[["formula"]])
  
  mf = tryCatch({
    eval(mf,parent.frame())
  },error = function(e){
    NULL
  })
  
  temp = map_lgl(mf , ~is.factor(.x))
  if(sum(temp)>0){
    stop("Categorical variables are not allowed in Z or Y")
  }
  
  mf.xf = tryCatch({
    eval(mf.xf,parent.frame())
  },error = function(e){
    NULL
  })
  
  mt <- attr(mf.xf, "terms")
  
  if(is.null(mf)){
    stop("Z should not be NULL")
  }
  else{
    ydat = model.response(mf)
  }
  
  if(!is.null(mf.xf))
  {
    xdat = model.matrix(mt, mf.xf, NULL)
    xdat = as.matrix(xdat[,2:dim(xdat)[2]])
  }else{
    xdat = mf.xf
  }
  
  zdat = mf[, chromoly[[3]], drop = FALSE]
  
  ydat = data.matrix(ydat)
  
  if(!is.null(xdat) & is.null(dim(xdat[,1]))){
    xdat = data.matrix(xdat)
  }
  else if(!is.null(dim(xdat[,1]))){
    xdat = xdat[,1]
  }
  
  if(is.null(dim(zdat[,1]))){
    zdat = data.matrix(zdat)
  }
  else{
    zdat = zdat[,1]
  }
  
  fit = plsim.est(xdat = xdat, zdat = zdat, ydat = ydat, ...)
  
  return(fit)
}

plsim.est.default = function(xdat=NULL, zdat, ydat, h=NULL, zetaini=NULL, MaxStep = 200L,
                             ParmaSelMethod="SimpleValidation",TestRatio=0.1,K = 3,
                             seed=0,verbose=TRUE, ...)
{
  
  
  data = list(x=xdat,y=ydat,z=zdat)
  x = data$x
  y = data$y
  z = data$z
  
  is.null( .assertion_for_variables(data))
  
  tempz = map_lgl(z , ~is.factor(.x))
  tempy = map_lgl(y , ~is.factor(.x))
  if((sum(tempz)>0)|(sum(tempy)>0)){
    stop("Categorical variables are not allowed in Z or Y")
  }
  
  if(!is.null(x)){
    x = model.matrix(~., as.data.frame(x))
    x = as.matrix(x[,2:dim(x)[2]])
  }
  
  if(is.data.frame(x))
    x = data.matrix(x)
  
  if(is.data.frame(z))
    z = data.matrix(z)
  
  if(is.data.frame(y))
    y = data.matrix(y)
  
  if(is.null(zetaini))
  {
    zetaini = plsim.ini(y~x|z,verbose=verbose)
  }
  
  if(is.null(x))
  {
    #x = matrix()
    flag = 0
    
    if(length(zetaini) > ncol(z))
    {
      zetaini = zetaini[1:ncol(z)]
    }
  }
  else
  {
    flag = 1
  }
  
  
  if( is.vector(h) & length(h) > 1  )
  {
    hVec = h
    
    res_plsimest_simple = plsim.bw(y~x|z,bandwidthList=hVec,
                                   TargetMethod="plsimest",zeta_i=zetaini,
                                   ParmaSelMethod=ParmaSelMethod,TestRatio=TestRatio,
                                   K=K,seed=seed,verbose=verbose)
    h = res_plsimest_simple$bandwidthBest       
  }
  else if(is.null(h))
  {
    
    res_plsimest_simple = plsim.bw(y~x|z,
                                   TargetMethod="plsimest",zeta_i=zetaini,
                                   ParmaSelMethod=ParmaSelMethod,TestRatio=TestRatio,
                                   K=K,seed=seed,verbose=verbose)
    h = res_plsimest_simple$bandwidthBest    
  }
  
  
  
  
  if(is.null(x))
  {
    x_tmp = matrix()
    res = .plsimestCore(x_tmp, y, z, h, zetaini, MaxStep,flag)
  }
  else
  {
    res = .plsimestCore(x, y, z, h, zetaini, MaxStep,flag)
  }
  
  
  data = list(x=x,y=y,z=z,h=h,
              zetaini=zetaini,MaxStep=MaxStep,
              SiMflag = !as.logical(flag))
  
  y_hat = res$y_hat
  
  fit = list(zeta=res$zeta,data=data,eta=res$eta,
             Z_alpha=z%*%matrix(res$zeta[1:ncol(z)]),
             r_square=.r_square(y,y_hat),mse=res$mse,
             variance=res$variance,stdzeta=res$stdzeta,
             y_hat=y_hat,call=match.call())
  
  class(fit) = "pls"
  
  return(fit)
}
