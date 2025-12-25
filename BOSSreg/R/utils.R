### functions that are called by the main functions, but invisible to users unless using namespace ':::'

## standardize the data ------------------------------------------------------------
# standardize x to be mean 0 and norm 1
# standardize y to be mean 0
std <- function(x, y, intercept){
  n = dim(x)[1]
  p = dim(x)[2]
  if(intercept){
    mean_x = Matrix::colMeans(x)
    mean_y = mean(y)

    x = scale(x, center = mean_x, scale = FALSE)
    y = scale(y, center = mean_y, scale = FALSE)
  }else{
    mean_x = rep(0, p)
    mean_y = 0
  }
  sd_demeanedx = sqrt(Matrix::colSums(x^2))
  x = scale(x, center = FALSE, scale = sd_demeanedx)

  return(list(x_std = x, y_std = y, mean_x=mean_x, sd_demeanedx=sd_demeanedx, mean_y=mean_y))
}

## calculate various information criteria: AIC, BIC, AICc, BICc, GCV, Cp -----------
calc.ic.all <- function(coef, x, y, df, sigma=NULL){
  # unify dimensions
  y = matrix(y, ncol=1)
  df = matrix(df, nrow=1)
  # sanity check
  if(ncol(coef) != ncol(df)){
    stop('the number of coef vectors does not match the number of df')
  }
  if(is.null(sigma)){
    stop("need to specify sigma for Mallow's Cp")
  }

  n = nrow(y)
  nfit = ncol(coef)
  fit = x %*% coef
  rss = Matrix::colSums(sweep(fit, 1, y, '-')^2)

  ic = list()
  ic$aic = log(rss/n)  + 2*df/n
  ic$bic = log(rss/n)  + log(n)*df/n
  ic$gcv = rss / (n-df)^2
  ic$cp = rss + 2*sigma^2*df

  # for AICc and BICc df larger than n-2 will cause trouble, round it
  df[which(df>=n-2)] = n-3
  ic$aicc = log(rss/n) + 2*(df+1)/(n-df-2)
  ic$bicc = log(rss/n) + log(n)*(df+1)/(n-df-2)
  # ic$bicc = log(rss/n) + df*(log(n)+2*log(p))/n
  return(ic)
}

## Heuristic df for BOSS ------------------------------------------------------------
# @param Q An orthogonal matrix, with \code{nrow(Q)=length(y)=n} and
#   \code{ncol(Q)=p}. For BOSS, Q is obtained by QR decomposition upon
#   an ordered design matrix.
# @param y A vector of response variable, with \code{length(y)=n}.
# @param sigma,mu The standard deviation and mean vector of the true model.
#   In practice, if not specified, they are calculated via full multiple
#   regression of y upon Q.
# @param x is required for the case where n<=p and sigma is estimated via lasso estimate

calc.hdf <- function(Q, y, sigma=NULL, mu=NULL, x=NULL){
  n = dim(Q)[1]
  p = dim(Q)[2]
  # if(p>=n){
  #   stop('hdf is undefined when p>=n')
  # }

  if(n <= p & is.null(x)){
    stop("This is the n<p scenario. Need the design matrix X for the lasso estimated sigmahat.")
  }

  # mu is estimated via multiple LS regression
  beta_hat = t(Q) %*% y # the multiple regression coef
  if(is.null(mu)){
    xtmu = beta_hat
  }else{
    xtmu = t(Q)%*%mu
  }
  xtmu_matrix = matrix(rep(xtmu,each=p-1), ncol=p-1, byrow=TRUE)

  if(is.null(sigma)){
    # For n>p, the estimated sigma is based on multiple LS regression
    # For n<=p, the estimated sigma is based on lasso with 10-fold CV
    if(n > p){
      resid = y - Q %*% beta_hat
      sigma = sqrt(sum(resid^2)/(n-p))
    }else{
      sigma = est.sigma.lasso(x, y)
    }
  }
  tryCatch({
    # calculate the inverse function of E(k(lambda))=k, where k=1,...p-1
    inverse = function(f, lower, upper) {
      function(y) stats::uniroot(function(x){f(x) - y}, lower=lower, upper=upper)[1]
    }
    exp_size <- function(x){
      c = stats::pnorm((x-xtmu) / sigma)
      d = stats::pnorm((-x-xtmu) / sigma)
      return( sum(1 - c + d) )
    }
    inverse_exp_size = inverse(exp_size, 0, 100*max(abs(xtmu)))
    sqrt_2lambda = unlist(lapply(1:(p-1), inverse_exp_size))
    sqrt_2lambda_matrix = matrix(rep(sqrt_2lambda,each=p), nrow=p, byrow=F)

    # plug the sequence of lambda into the expression of df(lambda)
    a = stats::dnorm((sqrt_2lambda_matrix-xtmu_matrix) / sigma)
    b = stats::dnorm((-sqrt_2lambda_matrix-xtmu_matrix) / sigma)

    size = 1:(p-1)
    sdf = (sqrt_2lambda/sigma) * Matrix::colSums(a + b)
    df = size + sdf
    names(df) = NULL
    return(list(hdf=c(0, df, p), sigma=sigma))
  }, error=function(e){
    warning('returns the df for a null model')
    return(list(hdf=c(0, 1:p + 2*p*stats::qnorm(1-1:p/(2*p))*stats::dnorm(stats::qnorm(1-1:p/(2*p)))), sigma=sigma))
  })
}

## Estimate sigma based on lasso-cv
est.sigma.lasso <- function(x, y){
  n = dim(x)[1]
  lasso_cv = glmnet::cv.glmnet(x, y, intercept=FALSE)
  betahat = glmnet::coef.glmnet(lasso_cv, s='lambda.min')[-1,]
  df = sum(betahat != 0)
  y_hat = x %*% betahat
  sigma_hat = sqrt( sum((y-y_hat)^2) / (n-df-1) )
  return(sigma_hat)
}
