
#' @name plsim.ini
#' @aliases plsim.ini
#' @aliases plsim.ini.formula
#' @aliases plsim.ini.default
#' 
#' @title Initialize coefficients
#'
#' @description Xia \emph{et al.}'s MAVE method is used to obtain initialized 
#' coefficients \eqn{\alpha_0} and \eqn{\beta_0} for PLSiM 
#' \deqn{Y = \eta(Z^T\alpha) + X^T\beta + \epsilon}. 
#' 
#' @usage plsim.ini(\dots)
#' 
#' \method{plsim.ini}{formula}(formula, data, \dots)
#' 
#' \method{plsim.ini}{default}(xdat, zdat, ydat, Method="MAVE_ini", verbose = TRUE, \dots)
#' 
#' @param formula a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param xdat input matrix (linear covariates). The model reduces to a single index model when \code{x} is NULL.
#' @param zdat input matrix (nonlinear covariates). \code{z} should not be NULL.
#' @param ydat input vector (response variable).
#' @param Method string, optional (default="MAVE_ini").
#' @param verbose bool, default: TRUE. Enable verbose output.
#' @param \dots additional arguments.
#' 
#' @return
#' \item{zeta_i}{initial coefficients. \code{zeta_i[1:ncol(z)]} is the initial coefficient vector 
#' \eqn{\alpha_0}, and \code{zeta_i[(ncol(z)+1):(ncol(z)+ncol(x))]} is the initial 
#' coefficient vector \eqn{\beta_0}.}
#'
#' @export
#'
#' @examples
#' 
#' # EXAMPLE 1 (INTERFACE=FORMULA)
#' # To obtain initial values by using MAVE methods for partially
#' # linear single-index model.
#' 
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1,2,1)
#' alpha = alpha/norm(alpha,"2")
#' 
#' beta = matrix(4,1,1)
#' 
#' # Case1: Matrix Input
#' x = matrix(1,n,1)
#' z = matrix(runif(n*2),n,2)
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' zeta_i = plsim.ini(y~x|z)
#' 
#' # Case 2: Vector Input
#' x = rep(1,n)
#' z1 = runif(n)
#' z2 = runif(n) 
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' 
#' zeta_i = plsim.ini(y~x|z1+z2)
#' 
#' 
#' # EXAMPLE 2 (INTERFACE=DATA FRAME)
#' # To obtain initial values by using MAVE methods for partially
#' # linear single-index model.
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
#' zeta_i = plsim.ini(xdat=X, zdat=Z, ydat=y)
#'
#' @references 
#' Y. Xia, W. Härdle. \emph{Semi-parametric estimation of partially linear single-index models}.
#' Journal of Multivariate Analysis, 2006, 97(5): 1162-1184.
#'
plsim.ini = function(...)
{
    UseMethod("plsim.ini")
}

plsim.ini.formula = function(formula,data,...)
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
  
  zeta_i = plsim.ini(xdat = xdat, zdat = zdat, ydat = ydat, ...)
  
  return(zeta_i)
}


plsim.ini.default = function(xdat, zdat, ydat, 
                            Method="MAVE_ini", verbose = TRUE,...)
{
  if(verbose)
  {
    cat('\n Utilize the MAVE_ini method to initialize coefficients\n')
  }
  
  data = list(x=xdat,y=ydat,z=zdat)
  is.null( .assertion_for_variables(data))
  class(data) = Method
  
  x = data$z
  z = data$x
  y = data$y
  
  tempz = map_lgl(x , ~is.factor(.x))
  tempy = map_lgl(y , ~is.factor(.x))
  
  if((sum(tempz)>0)|(sum(tempy)>0)){
    stop("Categorical variables are not allowed in Z or Y")
  }
  
  if(!is.null(z)){
    z = model.matrix(~., as.data.frame(z))
    z = as.matrix(z[,2:dim(z)[2]])
  }
  
  if(is.data.frame(x))
    x = data.matrix(x)
  
  if(is.data.frame(z))
    z = data.matrix(z)
  
  if(is.data.frame(y))
    y = data.matrix(y)
  
  n = nrow(x)
  dx = ncol(x)
  
  if( is.null(z) )
  {
    dz = 0
  }
  else
  {
    dz = ncol(z)
  }
  
  
  d_xz = dx + dz
  
  onep = matrix(1,dx,1)
  onen = matrix(1,n,1)
  
  B = diag(c(onep))
  nd = 1
  
  a = matrix(0,n,1)
  h2 = 2*n^(-2/(dx+4))
  
  eyep1 = diag(c(matrix(1,dx+1,1)))/n^2
  
  
  for(iter in 1:dx)
  {
    
    ye = y - a
    
    if( !is.null(z) )
    {
      fit = lm(ye~z-1)
      beta = as.matrix(fit$coefficients)
      ye = ye - z%*%beta
    }
    
    ab = matrix(1,dx,n)
    
    for(i in 1:n)
    {
      xi = x - t(.reshapeMatrix(matrix(x[i,]),n))
      kernel_xB = exp(-rowSums((xi%*%B)^2)/h2)
      onexi = cbind(onen,xi)
      xk = onexi*.reshapeMatrix(matrix(kernel_xB),dx+1)
      abi = solve(t(xk)%*%onexi+eyep1)%*%t(xk)%*%ye
      ab[,i] = abi[2:(dx+1)]
      a[i] = abi[1]
      
    }
    
    eigen_result = eigen(ab%*%t(ab))
    B0 = eigen_result$vectors
    D = eigen_result$values
    
    idx = order(D)
    B = B0
    B0 = B[,idx]
    
    if(dx == 1) B0 = matrix(B0)
    
    B[,1:nd] = B0[,dx:(dx-nd+1)]
    B = B[,1:max(1,dx-iter)]
    
  }
  
  alpha_i = B
  alpha_i = alpha_i/norm(alpha_i,"2")*sign(alpha_i[1])
  
  if( !is.null(z) ) beta_i = beta;
  
  if( !is.null(z) )
  {
    zeta_i = cbind(t(matrix(alpha_i)),t(matrix(beta_i)))
  }
  else
  {
    zeta_i = t(matrix(alpha_i))
  }
  
  return(zeta_i)
}

