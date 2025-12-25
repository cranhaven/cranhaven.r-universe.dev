

#' @name plsim.bw
#' @aliases plsim.bw
#' @aliases plsim.bw.formula
#' @aliases plsim.bw.default
#' @aliases bwsel_Core
#' @aliases bwsel_new.CrossValidation
#' @aliases bwsel_new.default
#' @aliases deal_formula
#' @aliases summary.pls
#' 
#' @title select bandwidth
#'
#' @description Select bandwidth for methods, including MAVE, Profile Least Squares Estimator and 
#' Penalized Profile Least Squares Estimator by cross validation or simple validation.
#'
#' @usage plsim.bw(\dots)
#' 
#' \method{plsim.bw}{formula}(formula, data, \dots)
#' 
#' \method{plsim.bw}{default}(xdat, zdat, ydat, zeta_i=NULL, bandwidthList=NULL, 
#' ParmaSelMethod="CrossValidation", K=5, TestRatio=0.1, TargetMethod='plsimest',
#' lambda=NULL, l1_ratio=NULL, VarSelMethod = "SCAD", MaxStep = 1L, 
#' verbose=FALSE, seed=0, \dots)
#' 
#' @param formula a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param xdat input matrix (linear covariates). The model reduces to a single index model when \code{x} is NULL.
#' @param zdat input matrix (nonlinear covariates). \code{z} should not be NULL.
#' @param ydat input vector (response variable).
#' @param bandwidthList vector, candidate bandwidths.
#' @param TargetMethod string, optional (default: "plsimest"). 
#' target method to be selected bandwidth for, which could be "MAVE", "plsimest" and "plsim".
#' @param ParmaSelMethod string, optional (default: "CrossValidation"). 
#' Method to select bandwidth, which could be Cross Validation ("CrossValidation") 
#' and Simple Validation ("SimpleValidation").
#' @param K int, optional (default: 5). The number of folds for Cross Validation.
#' @param TestRatio double, optional (default: 0.1). The ratio of test data for Simple Validation.
#' @param zeta_i initial coefficients. It could be obtained by the function \code{\link{plsim.ini}}.
#' \code{zeta_i[1:ncol(z)]} is the initial coefficient vector \eqn{\alpha_0}, 
#' and \code{zeta_i[(ncol(z)+1):(ncol(z)+ncol(x))]} is the initial coefficient vector \eqn{\beta_0}.
#' @param lambda the parameter for the function \link{plsim.vs.soft}, default: NULL.
#' @param l1_ratio the parameter for the function \link{plsim.vs.soft}, default: NULL.
#' @param VarSelMethod the parameter for the function \link{plsim.vs.soft}, default : "SCAD".
#' @param MaxStep the parameter for the function \link{plsim.vs.soft}, default: 1.
#' @param verbose the parameter for the function \link{plsim.vs.soft}, default: FALSE.
#' @param seed int, default: 0.
#' @param \dots additional arguments.
#'
#' @return
#' \item{bandwidthBest}{selected bandwidth}
#' \item{mse}{mean square errors corresponding to the \code{bandwidthList} }
#'
#' @export
#'
#' @examples
#'
#' # EXAMPLE 1 (INTERFACE=FORMULA)
#' # To select bandwidth by cross validation and simple validation. 
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
#'
#' beta = matrix(4,1,1)
#'
#' x = matrix(1,n,1)
#' z = matrix(runif(n*2),n,2)
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#'
#' # Select bandwidth for profile least squares estimator by cross validation
#' res_plsimest_cross = plsim.bw(y~x|z,bandwidthList=c(0.02,0.04,0.06,0.08,0.10))
#' 
#' # Select bandwidth for profile least squares estimator by simple validation
#' res_plsimest_simple = plsim.bw(y~x|z,bandwidthList=c(0.02,0.04,0.06,0.08,0.10),
#'                             ParmaSelMethod="SimpleValidation")
#' 
#' # Select bandwidth for penalized profile least squares estimator by simple validation
#' res_plsim_simple = plsim.bw(y~x|z,bandwidthList=c(0.02,0.04,0.06,0.08,0.10),
#'                          ParmaSelMethod="SimpleValidation",TargetMethod="plsim",lambda=0.01)
#' 
#' 
#' # EXAMPLE 2 (INTERFACE=DATA FRAME)
#' # To select bandwidth by cross validation and simple validation. 
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
#' # Select bandwidth for profile least squares estimator by cross validation
#' res_plsimest_cross = plsim.bw(xdat=X,zdat=Z,ydat=y,bandwidthList=c(0.02,0.04,0.06,0.08,0.10))
#' 
#' # Select bandwidth for profile least squares estimator by simple validation
#' res_plsimest_simple = plsim.bw(xdat=X,zdat=Z,ydat=y,bandwidthList=c(0.02,0.04,0.06,0.08,0.10),
#'                             ParmaSelMethod="SimpleValidation")
#' 
#' # Select bandwidth for penalized profile least squares estimator by simple validation
#' res_plsim_simple = plsim.bw(xdat=X,zdat=Z,ydat=y,bandwidthList=c(0.02,0.04,0.06,0.08,0.10),
#'                          ParmaSelMethod="SimpleValidation",TargetMethod="plsim",lambda=0.01)
#'                                  
plsim.bw = function(...)
{
    UseMethod("plsim.bw")
}

plsim.bw.formula = function(formula,data,...)
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
  
  
  result = plsim.bw(xdat = xdat, zdat = zdat, ydat = ydat, ...)
  
  return(result)
}

plsim.bw.default = function(xdat,zdat,ydat,zeta_i=NULL,bandwidthList=NULL,ParmaSelMethod="CrossValidation",
                                 K=5,TestRatio=0.1,TargetMethod='plsimest',lambda=NULL,l1_ratio=NULL,
                                 VarSelMethod = "SCAD",MaxStep = 1L,verbose=FALSE,seed=0,...)
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
  
  if(is.null(zeta_i))
  {
    zeta_i = plsim.ini(x,z,y,verbose)
  }
  
  
  n = nrow(y)
  
  
  if(is.null(bandwidthList))
  {
    bandwidthList = seq(0.1/sqrt(n),2*sqrt(log(n)/n),length=20)
  }
  
  
  class(data) = ParmaSelMethod 
  
  bwsel_Core(data,K,bandwidthList,TargetMethod,zeta_i,
                     lambda,l1_ratio,VarSelMethod,MaxStep,verbose,TestRatio,seed
  )
}

bwsel_Core=function(data,K,bandwidthList,Method,zeta_i,
                            lambda, l1_ratio,VarSelMethod,
                            MaxStep,verbose,TestRatio,seed)
{
  UseMethod("bwsel_new")
}


bwsel_new.default=function(data,K,bandwidthList,Method,zeta_i,
                                   lambda, l1_ratio,VarSelMethod,MaxStep,verbose,TestRatio,seed)
{
  
  if(verbose)
  {
    cat(paste('\n Simple Validation for Bandwidth Selection for ',Method,'\n',sep=""))  
  }
  
  
  x = data$x
  z = data$z
  y = data$y
  
  if(is.data.frame(x))
    x = data.matrix(x)
  
  if(is.data.frame(z))
    z = data.matrix(z)
  
  if(is.data.frame(y))
    y = data.matrix(y)
  
  n = nrow(y)
  dz = ncol(z)
  
  if(is.null(x))
  {
    dx = 0
  }
  else
  {
    dx = ncol(x) 
  }
  
  set.seed(seed)
  testIdx = sample(n,n*TestRatio)
  trainIdx = setdiff(1:n,testIdx)
  
  if(is.null(x))
  {
    x_train = NULL
  }
  else
  {
    x_train = matrix(x[trainIdx,],length(trainIdx),dx) 
  }
  z_train = matrix(z[trainIdx,],length(trainIdx),dz)
  y_train = matrix(y[trainIdx,])
  
  if(is.null(x))
  {
    x_test = NULL
  }
  else
  {
    x_test = matrix(x[testIdx,],length(testIdx),dx) 
  }
  z_test = matrix(z[testIdx,],length(testIdx),dz)
  y_test = matrix(y[testIdx,])
  
  mse = vector(length = length(bandwidthList),mode = "numeric")
  
  for(j in 1:length(bandwidthList))
  {
    
    if(Method=="plsimest")
    {
      res = plsim.est(x_train, z_train, y_train, bandwidthList[j], zeta_i,seed = seed)
    }
    else if(Method=="MAVE")
    {
      res = plsim.MAVE(x_train,z_train,y_train,bandwidthList[j],zeta_i,seed = seed)
    }
    else if(Method=="plsim")
    {
      if( is.null(lambda) )
      {
        stop("Lambda should not be NULL for plsim")
      }
      
      res = plsim.vs.soft(x_train,z_train,y_train,bandwidthList[j],zeta_i,lambda,
                  l1_ratio,MaxStep,VarSelMethod,verbose,seed = seed)
    }
    
    
    if(is.null(res))
      return(NULL)
    
    zeta = res$zeta
    eta = res$eta
    
    pred_res = predict(res, x_test, z_test)
    mse[j] = mean((y_test-pred_res)^2)
    
  }
  
  
  names(mse) = bandwidthList
  
  result = list()
  result$mse=mse
  result$bandwidthBest = bandwidthList[which.min(mse)]
  return(result)
  
}


bwsel_new.CrossValidation=function(data,K,bandwidthList,Method,zeta_i,
                                           lambda, l1_ratio,VarSelMethod,MaxStep,
                                           verbose,TestRatio,seed)
{
  
  if(verbose)
  {
    cat(paste('\n Cross Validation for Bandwidth Selection for ', Method, sep=""))  
    cat('\n\n')    
  }
  
  
  
  x = data$x
  z = data$z
  y = data$y
  
  if(is.data.frame(x))
    x = data.matrix(x)
  
  if(is.data.frame(z))
    z = data.matrix(z)
  
  if(is.data.frame(y))
    y = data.matrix(y)
  
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
  
  IdxList = list()
  IdxRemaining = 1:n
  
  for(i in 1:K)
  {
    set.seed(seed)
    IdxSelected = sample(n,n*1/K)
    IdxRemaining = setdiff(IdxRemaining,IdxSelected)
    IdxList[[i]] = IdxSelected
  }
  
  
  mse = vector(length = length(bandwidthList),mode = "numeric")
  
  for(j in 1:length(bandwidthList))
  {
    mseTmp = vector(length = K,mode = "numeric")
    
    for(i in 1:K)
    {
      testIdx = IdxList[[i]]
      trainIdx = setdiff(1:n,testIdx)
      
      if(is.null(x))
      {
        x_train = NULL
        x_test = NULL
      }
      else
      {
        x_train = matrix(x[trainIdx,],length(trainIdx),dx)
        x_test = matrix(x[testIdx,],length(testIdx),dx)        
      }
      
      
      y_train = matrix(y[trainIdx,])
      y_test = matrix(y[testIdx,])
      
      z_train = matrix(z[trainIdx,],length(trainIdx),dz)
      z_test = matrix(z[testIdx,],length(testIdx),dz)
      
      
      if(Method=="plsimest")
      {
        res = plsim.est(x_train,z_train,y_train,bandwidthList[j],zeta_i)
      }
      else if(Method=="MAVE")
      {
        res = plsim.MAVE(x_train,z_train,y_train,bandwidthList[j],zeta_i)
      }
      else if(Method=="plsim")
      {
        if( is.null(lambda) )
        {
          stop("Lambda should not be NULL for plsim")
        }
        
        res = plsim.vs.soft(x_train,z_train,y_train,bandwidthList[j],zeta_i,lambda,
                    l1_ratio,MaxStep,VarSelMethod,verbose)
      }

      zeta = res$zeta
      eta = res$eta
      
      pred_res = predict(res, x_test, z_test)
      mseTmp[i] = mean((y_test-pred_res)^2)
      
    }
    
    mse[j] = mean(mseTmp)
  }
  
  
  names(mse) = bandwidthList
  
  result = list()
  result$mse=mse
  result$bandwidthBest = bandwidthList[which.min(mse)]
  
  class(result) = "bw"
  
  return(result)
}

