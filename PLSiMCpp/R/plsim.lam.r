

.AIC=function(mse,df,n)
{
  return(log(mse) + df*2/n)
}

.BIC=function(mse,df,n)
{
  return(log(mse) + df*log(n)/n)
}

.IC=function(mse,df,n,Method="BIC")
{
  if(Method == "BIC")
  {
    return(.BIC(mse,df,n))
  }
  else
  {
    return(.AIC(mse,df,n))
  }
}




#' @name plsim.lam
#' @aliases plsim.lam
#' @aliases plsim.lam.formula
#' @aliases plsim.lam.default
#' 
#' @title Select lambda for Penalized Profile Least Squares Estimator
#'
#' @description Use AIC or BIC to 
#' choose the regularization parameters for Penalized Profile least squares (PPLS) estimation.
#' 
#' @usage plsim.lam(\dots)
#' 
#' \method{plsim.lam}{formula}(formula, data, \dots)
#' 
#' \method{plsim.lam}{default}(xdat=NULL, ydat, zdat, h, zetaini=NULL, penalty="SCAD", 
#' lambdaList=NULL, l1_ratio_List=NULL, lambda_selector="BIC", verbose=TRUE, seed=0, \dots)
#' 
#' @param formula a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param xdat input matrix (linear covariates). The model reduces to a single index model when \code{x} is NULL.
#' @param zdat input matrix (nonlinear covariates). \code{z} should not be NULL.
#' @param ydat input vector (response variable).
#' @param h bandwidth.
#' @param zetaini initial coefficients, optional (default: NULL). It could be obtained by the function \code{\link{plsim.ini}}.
#' \code{zetaini[1:ncol(z)]} is the initial coefficient vector \eqn{\alpha_0},
#' and \code{zetaini[(ncol(z)+1):(ncol(z)+ncol(x))]} is the initial coefficient vector \eqn{\beta_0}.
#' @param penalty string, optional (default="SCAD"). It could be "SCAD", "LASSO" or "ElasticNet".
#' @param lambdaList candidates for lambda selection. \code{lambda} is a constant that multiplies the penalty term. 
#' If \code{lambdaList} is NULL, function \link{plsim.lam} will automatically set it.
#' @param l1_ratio_List candidates for l1_ratio selection. \code{l1_ratio} is a constant that balances the importances 
#' of L1 norm and L2 norm for "ElasticNet". If \code{l1_ratio_List} is NULL, function \link{plsim.lam} ranges from 0 to 1 with 
#' an increment 0.1.
#' @param lambda_selector the criterion to select lambda (and l1_ratio), default: "BIC".
#' @param verbose bool, default: TRUE. Enable verbose output.
#' @param seed int, default: 0.
#' @param \dots additional arguments.
#'
#'
#' @return
#' \item{goodness_best}{the AIC (or BIC) statistics with \code{lambda_best}.}
#' \item{lambda_best}{lambda selected by AIC or BIC.}
#' \item{l1_ratio_best}{l1_ratio selected by AIC or BIC.}
#' \item{lambdaList}{\code{lambdaList} automatically selected when inputting NULL.}
#' 
#' 
#' @export
#'
#' @examples
#'
#' # EXAMPLE 1 (INTERFACE=FORMULA)
#' # To select the regularization parameters based on AIC.
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
#' beta = matrix(4,1,1)
#'
#' x = matrix(1,n,1)
#' z = matrix(runif(n*2),n,2)
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#'
#'
#' fit_plsimest = plsim.est(y~x|z)
#' 
#' # Select the regularization parameters by AIC
#' res = plsim.lam(y~x|z,h=fit_plsimest$data$h,zetaini = fit_plsimest$zeta,
#'              lambda_selector='AIC')
#' 
#' 
#' # EXAMPLE 2 (INTERFACE=DATA FRAME)
#' # To select the regularization parameters based on AIC.
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
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
#' fit_plsimest = plsim.est(xdat=X,zdat=Z,ydat=y)
#' 
#' # Select the regularization parameters by AIC
#' res2 = plsim.lam(xdat=X,ydat=y,zdat=Z,h=fit_plsimest$data$h,
#'               zetaini = fit_plsimest$zeta, lambda_selector='AIC')
#'
#' @references 
#' H. Liang, X. Liu, R. Li, C. L. Tsai. \emph{Estimation and testing for partially linear single-index models}.
#' Annals of statistics, 2010, 38(6): 3811.
#'
#'
plsim.lam = function(...)
{
    UseMethod("plsim.lam")
}

plsim.lam.formula = function(formula,data,...)
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
  
  result = plsim.lam(xdat = xdat, ydat = ydat, zdat = zdat, ...)
  
  return(result)
}

plsim.lam.default = function(xdat=NULL,ydat,zdat,h,zetaini=NULL,penalty="SCAD",lambdaList=NULL,
                                   l1_ratio_List=NULL,lambda_selector="BIC",verbose=TRUE,seed=0,...)
{
  
  data = list(x=xdat,y=ydat,z=zdat)
  x = data$x
  y = data$y
  z = data$z
  
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
  
  if(is.null(h))
  {
    stop("Please input a value for the bandwidth (h)")
  }
  
  n = nrow(y)
  if(is.null(x))
  {
    dx = 0
  }
  else
  {
    dx = ncol(x) 
  }
  dz = ncol(z)
  
  a = zetaini[1:dz]
  if(is.null(x))
  {
    b = zetaini[1:dz]
  }
  else
  {
    b = zetaini[(dz+1):(dz+dx)] 
  }
  
  
  if(is.null(x))
  {
    x_tmp = matrix()
    etandder_result = .etandder(a,b,h,y,z,x_tmp,0)
  }
  else
  {
    etandder_result = .etandder(a,b,h,y,z,x) 
  }
  eta = etandder_result$eta
  
  if(is.null(x))
  {
    r = y - eta
  }
  else
  {
    r = y - eta - x%*%b 
  }
  sigma = sqrt(sum(r^2)/n)
  
  
  if(is.null(lambdaList))
  {
    lambdaList = sigma * seq(0.1/sqrt(n),2*sqrt(log(n)/n),length=30)
    lambdaList = c(0, lambdaList)
  }
  
  if(is.null(l1_ratio_List))
  {
    l1_ratio_List = seq(0,1,length=11)
  }
  
  
  if(penalty != "ElasticNet")
  {
    goodness = matrix(0,length(lambdaList),1)
    
    
    for(k in 1:length(lambdaList))
    {
      
      plsim_result = plsim.vs.soft(x,z,y,h,zetaini,lambdaList[k],NULL,1,penalty,verbose,seed=seed)
      
      if(is.null(plsim_result))
        return(NULL)
      
      beta = plsim_result$zeta
      mse = plsim_result$mse
      
      df = sum(beta!=0)
      
      
      goodness[k] = .IC(mse,df,n,lambda_selector)
      
      
      if(verbose)
      {
        cat(paste("\n lambda",lambdaList[k],sep = "="))
        cat("\n ")
        cat(paste(lambda_selector,goodness[k],sep = "="))
        cat("\n\n")
      }
    }
    
    goodness_min = min(goodness)
    index_selectorMin = which.min(goodness)
    lambda_best = lambdaList[index_selectorMin]
    
    result = list()
    result$goodness_best = goodness_min
    result$lambda_best = lambda_best
    result$lambdaList = lambdaList
    
  }
  else
  {
    goodness = matrix(0,length(lambdaList),length(l1_ratio_List))
    
    cat(paste("\n Select lambda and l1_ratio for penalized plsim according to",
               lambda_selector, sep = " "))
    cat("\n")
    
    for(k in 1:length(lambdaList))
    {
      for(t in 1:length(l1_ratio_List))
      {
        plsim_result = plsim.vs.soft(x,z,y,h,zetaini,lambdaList[k],l1_ratio_List[t],1,penalty,verbose,seed=seed)
        beta = plsim_result$zeta
        mse = plsim_result$mse
        
        df = sum(beta!=0)
        
        
        goodness[k,t] = .IC(mse,df,n,lambda_selector)
        
        
        if(verbose)
        {
          cat(paste("\n lambda",lambdaList[k],sep = "="))
          cat(paste("\n l1_ratio",l1_ratio_List[t],sep = "="))
          cat("\n ")
          cat(paste(lambda_selector,goodness[k,t],sep = "="))
          cat("\n\n")
        }
      }
    }
    
    goodness_min = min(goodness)
    index_selectorMin = arrayInd( which.min(goodness) ,dim(goodness),dimnames(goodness))
    lambda_best = lambdaList[index_selectorMin[1]]
    l1_ratio_best = l1_ratio_List[index_selectorMin[2]]
    
    result = list()
    result$goodness_best = goodness_min
    result$lambda_best = lambda_best
    result$l1_ratio_best = l1_ratio_best
    result$lambdaList = lambdaList
    
    class(result) = "lamsel"
  }
  
  return(result)
}
