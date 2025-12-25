

#' @name plsim.vs.hard
#' @aliases plsim.vs.hard
#' @aliases plsim.vs.hard.formula
#' @aliases plsim.vs.hard.default
#' @aliases stepWise
#' @aliases dropOneVar
#' @aliases varSelCore
#' @aliases varSelCore.PPLSE
#' @aliases varSelCore.StepWise
#' 
#' @title Variable Selection for Partial Linear Single Index Models
#' @description Variable Selection based on AIC, BIC, SCAD, LASSO and 
#' Elastic Net. The methods based on SCAD, LASSO and Elastic Net are implemented with Penalized Profile 
#' Least Squares Estimator, while AIC and BIC are implemented with Stepwise Regression.
#'
#' @usage plsim.vs.hard(\dots)
#' 
#' \method{plsim.vs.hard}{formula}(formula, data, \dots)
#' 
#' \method{plsim.vs.hard}{default}(xdat=NULL, zdat, ydat, h=NULL, zeta_i=NULL, 
#' lambdaList=NULL, l1RatioList=NULL, lambda_selector="BIC", threshold=0.05,
#' Method="SCAD", verbose=TRUE, ParmaSelMethod="SimpleValidation", seed=0, \dots)
#' 
#' @param formula a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param xdat input matrix (linear covariates). The model reduces to a single index model when \code{x} is NULL.
#' @param zdat input matrix (nonlinear covariates). \code{z} should not be NULL.
#' @param ydat input vector (response variable).
#' @param h a numerical value or a vector for bandwidth. If \code{h} is NULL, a default vector c(0.01,0.02,0.05,0.1,0.5)
#' will be set for it. \link{plsim.bw} is employed to select the optimal bandwidth when h is a vector or NULL.
#' @param zeta_i initial coefficients, optional (default: NULL). It could be obtained by the function \code{\link{plsim.ini}}.
#' \code{zeta_i[1:ncol(z)]} is the initial coefficient vector \eqn{\alpha_0},
#' and \code{zeta_i[(ncol(z)+1):(ncol(z)+ncol(x))]} is the initial coefficient vector \eqn{\beta_0}.
#' @param verbose bool, default: TRUE. Enable verbose output.
#' @param Method variable selection method, default: "SCAD". It could be "SCAD", "LASSO", "ElasticNet", "AIC" or "BIC".
#' @param lambdaList the parameter for the function \link{plsim.lam}, default: "NULL".
#' @param l1RatioList the parameter for the function \link{plsim.lam}, default: "NULL".
#' @param lambda_selector the parameter for the function \link{plsim.lam}, default: "BIC".
#' @param threshold the threshold to select important variable according to the estimated coefficients.
#' @param ParmaSelMethod the parameter for the function \link{plsim.bw}.
#' @param seed int, default: 0.
#' @param \dots additional arguments.
#'
#' @return
#' \item{alpha_varSel}{selected variables in \code{z}.}
#' \item{beta_varSel}{selected variables in \code{x}.}
#' \item{fit_plsimest}{\code{fit_plsimest} is not NULL when \code{h} is a vector or NULL. 
#' For each bandwidth, \link{plsim.est} is employed to integrate selected variabels. Finally, the optimal
#' fitted model will be selected according to BIC.}
#'
#' @export
#'
#' @examples
#'
#' # EXAMPLE 1 (INTERFACE=FORMULA)
#' # To select variables with Penalized Profile Least Squares Estimation based on 
#' # the penalty LASSO.
#' 
#' n = 50
#' dx = 10
#' dz = 5
#' sigma = 0.2
#' alpha = matrix(c(1,3,1.5,0.5,0),dz,1)
#' alpha = alpha/norm(alpha,"2")
#' beta = matrix(c(3,2,0,0,0,1.5,0,0.2,0.3,0.15),dx,1)
#' 
#' A = sqrt(3)/2-1.645/sqrt(12)
#' B = sqrt(3)/2+1.645/sqrt(12)
#' z = matrix(runif(n*dz),n,dz)
#' x = matrix(runif(n*dx),n,dx)
#' y = sin( (z%*%alpha - A) * 3.1415926 * (B-A) ) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#'
#' # Variable Selectioin Based on LASSO
#' res_varSel_LASSO = plsim.vs.hard(y~x|z,h=0.1,Method="LASSO")
#'
#'
#' # EXAMPLE 2 (INTERFACE=DATA FRAME)
#' # To select variables with Penalized Profile Least Squares Estimation based on 
#' # the penalty LASSO.
#' 
#' n = 50
#' dx = 10
#' dz = 5
#' sigma = 0.2
#' alpha = matrix(c(1,3,1.5,0.5,0),dz,1)
#' alpha = alpha/norm(alpha,"2")
#' beta = matrix(c(3,2,0,0,0,1.5,0,0.2,0.3,0.15),dx,1)
#' 
#' A = sqrt(3)/2-1.645/sqrt(12)
#' B = sqrt(3)/2+1.645/sqrt(12)
#' z = matrix(runif(n*dz),n,dz)
#' x = matrix(runif(n*dx),n,dx)
#' y = sin( (z%*%alpha - A) * 3.1415926 * (B-A) ) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' Z = data.frame(z)
#' X = data.frame(x)
#'
#' # Variable Selectioin Based on LASSO
#' res_varSel_LASSO = plsim.vs.hard(xdat=X,zdat=Z,ydat=y,h=0.1,Method="LASSO")
#' 
plsim.vs.hard = function(...)
{
    UseMethod("plsim.vs.hard")
}

plsim.vs.hard.formula = function(formula,data,...)
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
  
  
  res = plsim.vs.hard(xdat = xdat, zdat = zdat, ydat = ydat, ...)
  
  return(res)
}

plsim.vs.hard.default = function(xdat=NULL,zdat,ydat,h=NULL,zeta_i=NULL,lambdaList=NULL,
                          l1RatioList=NULL,lambda_selector="BIC",threshold=0.05,
                          Method="SCAD",verbose=TRUE,ParmaSelMethod="SimpleValidation",seed=0,...)
{
  n = nrow(ydat)
  
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
    if(verbose) zeta_i = plsim.ini(x,z,y,verbose=verbose)
  }    
  
  
  if(Method %in% c('SCAD','LASSO','ElasticNet') )
  {
    class(data) = 'PPLSE'  
  }
  else if(Method %in% c('AIC','BIC') )
  {
    class(data) = 'StepWise'
  }
  else
  {
    Method = "SCAD"
    class(data) = 'PPLSE'
  }
  
  
  if( !is.null(h) & length(h)==1 )
  {
    res = varSelCore(data,h,zeta_i,verbose,lambdaList,l1RatioList,
                     lambda_selector,threshold,Method,flag=FALSE,ParmaSelMethod,seed)
    
  }
  else if( is.vector(h) & length(h) > 1 )
  {
    hVec = h
    
    BIC_best = 1000
    res = NULL
    
    for(j in 1:length(hVec))
    {
      if(verbose)
      {
        cat(paste("\n----------Variable Selection when h=",
                  as.character(hVec[j]),"----------\n\n",sep=""))        
      }
      
      
      res_tmp = varSelCore(data,hVec[j],zeta_i,verbose,lambdaList,l1RatioList,
                           lambda_selector,threshold,Method,flag = TRUE,ParmaSelMethod,seed)
      
      
      if(verbose)
      {
        cat( paste("BIC: ",as.character(res_tmp$fit_plsimest$BIC),"\n", sep=""))        
      }
      
      
      if(res_tmp$fit_plsimest$BIC < BIC_best)
      {
        res = res_tmp
        BIC_best = res_tmp$fit_plsimest$BIC
      }
    }
    
  }
  else if( is.null(h) )
  {
    
    hVec = seq(0.1/sqrt(n),2*sqrt(log(n)/n),length=20)
    
    BIC_best = 1000
    res = NULL
    
    for(j in 1:length(hVec))
    {
      if(verbose)
      {
        cat(paste("\n----------Variable Selection when h=",
                  as.character(hVec[j]),"----------\n\n"), sep="")      
      }
      
      
      res_tmp = varSelCore(data,hVec[j],zeta_i,verbose,lambdaList,l1RatioList,
                           lambda_selector,threshold,Method,flag = TRUE,ParmaSelMethod,seed)
      
      if(is.null(res_tmp))
        return(NULL)
      
      if(verbose)
      {
        cat( paste(" BIC: ",as.character(res_tmp$fit_plsimest$BIC),sep=""))
        cat("\n")        
      }
      
      
      if(res_tmp$fit_plsimest$BIC < BIC_best)
      {
        res = res_tmp
        BIC_best = res_tmp$fit_plsimest$BIC
      }
    }
    
    
  }
  
  return(res)
  
}


varSelCore=function(data,h,zeta_i,verbose,lambdaList,l1RatioList,
                    lambda_selector,threshold,Method,flag,ParmaSelMethod,seed)
{
  UseMethod("varSelCore")
}

varSelCore.PPLSE=function(data,h,zeta_i,verbose,lambdaList,l1RatioList,
                          lambda_selector,threshold,Method,flag,ParmaSelMethod,seed)
{
  
  
  x = data$x
  z = data$z
  y = data$y
  
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
  
  
  res = plsim.lam(x,y,z,h,zeta_i,Method,lambdaList,l1RatioList,lambda_selector,verbose,seed)
  
  if(is.null(res))
    return(NULL)
  
  plsim_result = plsim.vs.soft(x,z,y,h,zeta_i,res$lambda_best,l1RatioList,1,Method,verbose,ParmaSelMethod,seed=seed)
  
  if(is.null(plsim_result))
    return(NULL)
  
  alpha = plsim_result$zeta[1:dz]
  if(!is.null(x))
  {
    beta = plsim_result$zeta[(dz+1):(dz+dx)]    
  }
  
  
  alpha_norm = abs(alpha)/max(abs(alpha))
  if(!is.null(x))
  {
    beta_norm = abs(beta)/max(abs(beta))    
  }
  
  
  alpha_eliminated = which(alpha_norm<threshold)
  if(!is.null(x))
  {
    beta_eliminated = which(beta_norm<threshold)    
  }
  
  
  res_alpha_sorted = sort.int(alpha_norm,index.return = TRUE,decreasing = TRUE)
  if(!is.null(x))
  {
    res_beta_sorted = sort.int(beta_norm,index.return = TRUE,decreasing = TRUE) 
  }
  
  
  alpha_varSel = setdiff(res_alpha_sorted$ix,alpha_eliminated) 
  if(!is.null(x))
  {
    beta_varSel = setdiff(res_beta_sorted$ix,beta_eliminated)     
  }
  
  
  if(verbose)
  {
    cat(paste("\n Important varaibles in Z are",
        paste(alpha_varSel,collapse = ","),sep = ": "))    
  }
  
  if(!is.null(x))
  {
    if( length(beta_varSel) > 0 )
    {
      if(verbose)
      {
        cat(paste("\n Important varaibles in X are",
                  paste(beta_varSel,collapse = ","),sep = ": "))        
      }
      
    }
  }
  
  if(verbose) cat("\n")
  
  
  if(!is.null(x))
  {
    result = list(alpha_varSel=alpha_varSel,beta_varSel=beta_varSel)    
  }
  else
  {
    result = list(alpha_varSel=alpha_varSel)
  }
  
  
  if(flag)
  {
    if(is.null(x))
    {
      x_vs = NULL
    }
    else
    {
      if(length(beta_varSel) > 1)
      {
        x_vs = x[,beta_varSel]
      }
      else if(length(beta_varSel) == 1)
      {
        x_vs = matrix(x[,beta_varSel])
      }
      else
      {
        x_vs = NULL
      }
    }
    
    
    if(length(alpha_varSel) > 1)
    {
      z_vs = z[,alpha_varSel]
    }
    else if( length(alpha_varSel) == 1)
    {
      z_vs = matrix(z[,alpha_varSel])
    }
    else
    {
      z_vs = NULL
    }
    
    
    fit_plsimest = plsim.est(x_vs, z_vs, y, ParmaSelMethod = ParmaSelMethod,
                            seed=seed,verbose=verbose)
    
    if( !is.null(fit_plsimest))
    {
      fit_plsimest$BIC = .IC(fit_plsimest$mse,sum(fit_plsimest$zeta!=0),n,"BIC")
      result$fit_plsimest = fit_plsimest      
    }
    else
    {
      fit_plsimest = list(BIC=10000)
      result$fit_plsimest = fit_plsimest
    }
    
  }
  
  
  return(result)
}



varSelCore.StepWise=function(data,h,zeta_i,verbose,lambdaList,l1RatioList,
                             lambda_selector,threshold,Method,flag,ParmaSelMethod,seed)
{
  
  x = data$x
  z = data$z
  y = data$y
  
  n = nrow(y)
  if(is.null(x))
  {
    dx = 0
  }
  else
  {
    dx = ncol(x) 
    colnames(x) = 1:dx
  }
  dz = ncol(z)
  colnames(z) = 1:dz
  
  res = stepWise(data,h,zeta_i,Method,seed,verbose)
  
  
  if(flag)
  {
    if(is.null(x))
    {
      x_vs = NULL
    }
    else
    {
      if(length(res$beta_varSel) > 1)
      {
        x_vs = x[,res$beta_varSel]
      }
      else
      {
        x_vs = matrix(x[,res$beta_varSel])
      }      
    }
    
    if(length(res$alpha_varSel) > 1)
    {
      z_vs = z[,res$alpha_varSel]
    }
    else
    {
      z_vs = matrix(z[,res$alpha_varSel])
    }
    
    fit_plsimest = plsim.est(x_vs, z_vs, y,seed=seed)
    fit_plsimest$BIC = .IC(fit_plsimest$mse,sum(fit_plsimest$zeta!=0),n,"BIC")
    res$fit_plsimest = fit_plsimest
  }  
  
  return(res)
  
}


stepWise=function(data,h,zeta_i,Method="BIC",seed,verbose)
{
  x = data$x
  z = data$z
  y = data$y
  
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
  
  
  if(!is.null(x))
  {
    colnames(x) = 1:dx 
  }
  colnames(z) = 1:dz
  
  
  
  res = plsim.est(x,z,y,h,zeta_i,seed=seed)
  zeta = res$zeta
  mse = res$mse
  
  df = sum(zeta!=0)
  
  IC_all = .IC(mse,df,n,Method)
  
  
  
  while (TRUE)
  {
    
    res = dropOneVar(x,y,z,h,zeta_i,Method,seed)
    IC_tmp = res$IC
    Component = res$Component
    Idx = res$Idx
    
    
    if(IC_tmp < IC_all)
    {
      IC_all = IC_tmp
      
      if(Component == "X")
      {
        if(ncol(x) == 1)
        {
          x = NULL
        }
        else if(ncol(x) == 2)
        {
          x = matrix(x[,-Idx])
        }
        else
        {
          x = x[,-Idx]
        }
        
        zeta_i = zeta_i[-(ncol(z)+Idx)]
      }
      else
      {
        if(ncol(z) == 2)
        {
          z = matrix(z[,-Idx])
        }
        else
        {
          z = z[,-Idx]
        }
        zeta_i = zeta_i[-Idx]
      }
    }
    else
    {
      break
    }
    
    if(!is.null(x))
    {
      if(verbose)
      {
        cat('\nSelected X:')
        cat(paste(colnames(x),collapse = ','))
        cat('\n')         
      }
      
    }
    if(verbose)
    {
      cat('\nSelected Z:')
      cat(paste(colnames(z),collapse = ','))
      cat('\n\n')         
    }
    
  }
  
  result = list()
  if(!is.null(x))
  {
    result$beta_varSel = colnames(x)
  }
  result$alpha_varSel = colnames(z)
  
  
  return(result)
}



dropOneVar=function(x,y,z,h,zeta_i,Method="BIC",seed)
{
  
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
  
  
  X_IC_list = c()
  
  i = 1
  while( i <= dx )
  {
    if(dx == 1)
    {
      x_tmp = NULL
    }
    if(dx == 2)
    {
      x_tmp = matrix(x[,-i])
    }
    else
    {
      x_tmp = x[,-i]
    }
    
    
    zeta_tmp = zeta_i[-(dz+i)]
    
    
    res = plsim.est(x_tmp,z,y,h,zeta_tmp,seed=seed)
    zeta = res$zeta
    mse = res$mse
    
    df = sum(zeta!=0)
    
    X_IC_list[i] = .IC(mse,df,n,Method)
    
    
    i = i + 1
    
    
  }
  
  if(!is.null(x))
  {
    X_IC_min = min(X_IC_list)
    X_IC_min_Idx = which.min(X_IC_list)    
  }
  else
  {
    X_IC_min = 10000
  }
  
  Z_IC_list = c()
  
  i = 1
  while( (i <= dz) & (dz > 1) )
  {
    
    
    if(dz == 2)
    {
      z_tmp = matrix(z[,-i])
    }
    else
    {
      z_tmp = z[,-i]
    }
    
    
    zeta_tmp = zeta_i[-i]
    
    res = plsim.est(x,z_tmp,y,h,zeta_tmp,seed=seed)
    zeta = res$zeta
    mse = res$mse
    
    df = sum(zeta!=0)
    
    
    Z_IC_list[i] = .IC(mse,df,n,Method)
    
    
    
    i = i + 1
  }
  
  Z_IC_min = min(Z_IC_list)
  Z_IC_min_Idx = which.min(Z_IC_list)
  
  
  result = list()
  
  if(X_IC_min < Z_IC_min)
  {
    result$IC = X_IC_min
    result$Component = "X"
    result$Idx = X_IC_min_Idx
  }
  else
  {
    result$IC = Z_IC_min
    result$Component = "Z"
    result$Idx = Z_IC_min_Idx
  }
  
  
  return(result)
}