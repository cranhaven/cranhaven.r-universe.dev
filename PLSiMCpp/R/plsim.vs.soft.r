

#' @name plsim.vs.soft
#' @aliases plsim.vs.soft
#' @aliases plsim.vs.soft.formula
#' @aliases plsim.vs.soft.default
#' 
#' @title Penalized Profile Least Squares Estimator
#' @description PPLS along with introducing penalty terms so as to simultaneously
#' estimate parameters and select important variables in PLSiM 
#' \deqn{Y = \eta(Z^T\alpha) + X^T\beta + \epsilon}.
#' 
#' @usage plsim.vs.soft(\dots)
#' 
#' \method{plsim.vs.soft}{formula}(formula, data, \dots)
#' 
#' \method{plsim.vs.soft}{default}(xdat=NULL, zdat, ydat, h=NULL, zetaini=NULL, 
#' lambda=0.01, l1_ratio=NULL, MaxStep = 1L, penalty = "SCAD", verbose=TRUE, 
#' ParmaSelMethod="SimpleValidation", TestRatio=0.1, K = 3, seed=0, \dots)
#' 
#' @param formula a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param xdat input matrix (linear covariates). The model reduces to a single index model when \code{x} is NULL.
#' @param zdat input matrix (nonlinear covariates). \code{z} should not be NULL.
#' @param ydat input vector (response variable).
#' @param h a value or a vector for bandwidth. If \code{h} is NULL, a default vector c(0.01,0.02,0.05,0.1,0.5)
#' will be set for it. \link{plsim.bw} is employed to select the optimal bandwidth when \code{h} is a vector or NULL.
#' @param zetaini initial coefficients, optional (default: NULL). It could be obtained by the function \code{\link{plsim.ini}}.
#' \code{zetaini[1:ncol(z)]} is the initial coefficient vector \eqn{\alpha_0},
#' and \code{zetaini[(ncol(z)+1):(ncol(z)+ncol(x))]} is the initial coefficient vector \eqn{{\beta}_0}.
#' @param MaxStep int, optional (default=1). Hard limit on iterations within solver.
#' @param lambda double. Constant that multiplies the penalty term.
#' @param l1_ratio double, default=NULL. It should be set with a value from the range \eqn{[0,1]}
#' when you choose "ElasticNet" for the parameter \code{penalty}.
#' @param penalty string, optional (default="SCAD"). It could be "SCAD", "LASSO" and "ElasticNet".
#' @param verbose bool, default: TRUE. Enable verbose output.
#' @param ParmaSelMethod the parameter for the function \link{plsim.bw}.
#' @param TestRatio the parameter for the function \link{plsim.bw}.
#' @param K the parameter for the function \link{plsim.vs.soft}.
#' @param seed int, default: 0.
#' @param \dots additional arguments.
#'
#' @return
#' \item{eta}{estimated non-parametric part \eqn{\hat{\eta}(Z^T{\hat{\alpha} })}.}
#' \item{zeta}{estimated coefficients. \code{zeta[1:ncol(z)]} is \eqn{\hat{\alpha}}, 
#' and \code{zeta[(ncol(z)+1):(ncol(z)+ncol(x))]} is \eqn{\hat{\beta}}.}
#' \item{y_hat}{ \code{y}'s estimates.}
#' \item{mse}{mean squared errors between y and \code{y_hat}.}
#' \item{data}{data information including \code{x}, \code{z}, \code{y}, bandwidth \code{h}, 
#' initial coefficients \code{zetaini}, iteration step \code{MaxStep}, flag \code{SiMflag}, 
#' \code{penalty}, \code{lambda} and \code{l1_ratio}.
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
#' # To estimate parameters of partially linear single-index model and select 
#' # variables using different penalization methods such as SCAD, LASSO, ElasticNet.
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
#' # Compute the penalized profile least-squares estimator with the SCAD penalty
#' fit_scad = plsim.vs.soft(y~x|z,lambda = 0.01)
#' summary(fit_scad)
#' 
#' # Compute the penalized profile least-squares estimator with the LASSO penalty
#' fit_lasso = plsim.vs.soft(y~x|z,lambda = 1e-3, penalty = "LASSO")
#' summary(fit_lasso)
#' 
#' # Compute the penalized profile least-squares estimator with the ElasticNet penalty
#' fit_enet = plsim.vs.soft(y~x|z,lambda = 1e-3, penalty = "ElasticNet")
#' summary(fit_enet)
#' 
#' # Case 2: Vector Input
#' x = rep(1,n)
#' z1 = runif(n)
#' z2 = runif(n) 
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' # Compute the penalized profile least-squares estimator with the SCAD penalty
#' fit_scad = plsim.vs.soft(y~x|z1+z2,lambda = 0.01)
#' summary(fit_scad)
#' 
#' # Compute the penalized profile least-squares estimator with the LASSO penalty
#' fit_lasso = plsim.vs.soft(y~x|z1+z2,lambda = 1e-3, penalty = "LASSO")
#' summary(fit_lasso)
#' 
#' # Compute the penalized profile least-squares estimator with the ElasticNet penalty
#' fit_enet = plsim.vs.soft(y~x|z1+z2,lambda = 1e-3, penalty = "ElasticNet")
#' summary(fit_enet)
#'
#' # EXAMPLE 2 (INTERFACE=DATA FRAME)
#' # To estimate parameters of partially linear single-index model and select 
#' # variables using different penalization methods such as SCAD, LASSO, ElasticNet.
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
#' # Compute the penalized profile least-squares estimator with the SCAD penalty
#' fit_scad = plsim.vs.soft(xdat=X,zdat=Z,ydat=y,lambda = 0.01)
#' summary(fit_scad)
#' 
#' # Compute the penalized profile least-squares estimator with the LASSO penalty
#' fit_lasso = plsim.vs.soft(xdat=X,zdat=Z,ydat=y,lambda = 1e-3, penalty = "LASSO")
#' summary(fit_lasso)
#' 
#' # Compute the penalized profile least-squares estimator with the ElasticNet penalty
#' fit_enet = plsim.vs.soft(xdat=X,zdat=Z,ydat=y,lambda = 1e-3, penalty = "ElasticNet")
#' summary(fit_enet)
#' 
#' @references
#'
#' H. Liang, X. Liu, R. Li, C. L. Tsai. \emph{Estimation and testing for partially linear single-index models}.
#' Annals of statistics, 2010, 38(6): 3811.
#' 
plsim.vs.soft = function(...)
{
    UseMethod("plsim.vs.soft")
}

plsim.vs.soft.formula = function(formula,data,...)
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
  
  
  fit = plsim.vs.soft(xdat = xdat, zdat = zdat, ydat = ydat, ...)
  
  return(fit)
}


plsim.vs.soft.default = function(xdat=NULL, zdat, ydat, h=NULL, zetaini=NULL, lambda=0.01, l1_ratio=NULL, MaxStep = 1L,
                         penalty = "SCAD", verbose=TRUE,
                         ParmaSelMethod="SimpleValidation",TestRatio=0.1,K = 3,seed=0,...)
{
  data = list(x=xdat,y=ydat,z=zdat)
  x = xdat
  z = zdat
  y = ydat
  .assertion_for_variables(data)
  
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
    zetaini = plsim.ini(x, z, y, verbose = verbose)
  }
  
  if(is.null(x))
  {
    SiMflag = 0
    
    if(length(zetaini) > ncol(z))
    {
      stop("The dimension of zetaini is not equal to that of covariates")
    }
  }
  else
  {
    SiMflag = 1
  }
  
  if(verbose)
  {
    cat(paste("\n Adopt ",penalty," pentaly function\n\n",sep=""))
  }
  
  if(is.null(l1_ratio) & penalty == "ElasticNet")
  {
    cat("l1_ratio is set as 0.1 for ElasticNet for default.\n\n")  
    l1_ratio = 0.1
  }
  else
  {
    l1_ratio = -1 
  }    
  
  
  
  if( is.vector(h) & length(h) > 1 )
  {
    hVec = h
    
    res_plsim_simple = plsim.bw(x,z,y,bandwidthList=hVec,
                                     TargetMethod="plsim",zeta_i=zetaini,
                                     ParmaSelMethod=ParmaSelMethod,TestRatio=TestRatio,K=K,lambda=lambda,seed=seed)
    
    if(is.null(res_plsim_simple))
      return(NULL)
    
    h = res_plsim_simple$bandwidthBest
  }
  else if(is.null(h))
  {
    res_plsim_simple = plsim.bw(x,z,y,
                                     TargetMethod="plsim",zeta_i=zetaini,
                                     ParmaSelMethod=ParmaSelMethod,TestRatio=TestRatio,K=K,lambda=lambda,seed=seed)
    if(is.null(res_plsim_simple))
      return(NULL)
    
    h = res_plsim_simple$bandwidthBest    
  }
  
  
  
  
  if(is.null(x))
  {
    x_tmp = matrix()
    result = .plsimCore(x_tmp,y,z,h,zetaini,lambda,l1_ratio,MaxStep,penalty,SiMflag,verbose)
  }
  else
  {
    result = .plsimCore(x,y,z,h,zetaini,lambda,l1_ratio,MaxStep,penalty,SiMflag,verbose) 
  }
  
  if(length(result)==1)
  {
    stop("Invoked with improper input, please see plsim.vs.soft documentation for proper use")
  }
  
  data = list(x=x,y=y,z=z,h=h,lambda=lambda,l1_ratio=l1_ratio,
              zetaini=zetaini,MaxStep=MaxStep,
              SiMflag = !as.logical(SiMflag),penalty = penalty)
  
  
  y_hat = result$y_hat
  
  
  fit = list(zeta=result$zeta,data=data,eta=result$eta,
             Z_alpha=z%*%matrix(result$zeta[1:ncol(z)]),
             r_square=.r_square(y,y_hat),mse=result$mse,
             variance=result$variance,stdzeta=result$stdzeta,
             y_hat=y_hat)
  
  
  class(fit) = "pls"
  
  return(fit)
}
