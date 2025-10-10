#' Function to implement the horseshoe shrinkage prior in Bayesian linear regression
#'
#'
#' This function employs the algorithm proposed in Bhattacharya et al. (2016).
#' The global-local scale parameters are updated via a slice sampling scheme given
#'  in the online supplement of Polson et al. (2014). Two different algorithms are
#'  used to compute posterior samples of the \eqn{p*1} vector of regression coefficients \eqn{\beta}.
#'  The method proposed in Bhattacharya et al. (2016) is used when \eqn{p>n}, and the
#'  algorithm provided in Rue (2001) is used for the  case \eqn{p<=n}. The function
#'  includes options for full hierarchical Bayes versions with hyperpriors on all
#'  parameters, or empirical Bayes versions where some parameters are taken equal
#'  to a user-selected value.
#'
#'  The model is:
#' \deqn{y=X\beta+\epsilon, \epsilon \sim N(0,\sigma^2)}
#'
#' The full Bayes version of the horseshoe, with hyperpriors on both \eqn{\tau} and \eqn{\sigma^2} is:
#'
#' \deqn{\beta_j \sim N(0,\sigma^2 \lambda_j^2 \tau^2)}
#' \deqn{\lambda_j \sim Half-Cauchy(0,1), \tau \sim Half-Cauchy (0,1)}
#' \deqn{\sigma^2 \sim 1/\sigma^2}
#'
#' There is an option for a truncated Half-Cauchy prior (truncated to [1/p, 1]) on \eqn{\tau}.
#' Empirical Bayes versions are available as well, where \eqn{\tau} and/or
#' \eqn{\sigma^2} are taken equal to fixed values, possibly estimated using the data.
#'
#' @references Bhattacharya A., Chakraborty A., and Mallick B.K (2016), Fast sampling
#' with Gaussian scale-mixture priors in high-dimensional regression.
#' Biometrika 103(4), 985–991.
#'
#' Polson, N.G., Scott, J.G. and Windle, J. (2014) The Bayesian Bridge.
#' Journal of Royal Statistical Society, B, 76(4), 713-733.
#'
#' Rue, H. (2001). Fast sampling of Gaussian Markov random fields. Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 63, 325–338.
#'
#' Carvalho, C. M., Polson, N. G., and Scott, J. G. (2010), The Horseshoe
#'  Estimator for Sparse Signals. Biometrika 97(2), 465–480.
#'
#'@param y Response, a \eqn{n*1} vector.
#'@param X Matrix of covariates, dimension \eqn{n*p}.
#'@param method.tau Method for handling \eqn{\tau}. Select "truncatedCauchy" for full
#' Bayes with the Cauchy prior truncated to [1/p, 1], "halfCauchy" for full Bayes with
#' the half-Cauchy prior, or "fixed" to use a fixed value (an empirical Bayes estimate,
#' for example).
#'@param tau  Use this argument to pass the (estimated) value of \eqn{\tau} in case "fixed"
#' is selected for method.tau. Not necessary when method.tau is equal to"halfCauchy" or
#' "truncatedCauchy". The default (tau = 1) is not suitable for most purposes and should be replaced.
#'@param method.sigma Select "Jeffreys" for full Bayes with Jeffrey's prior on the error
#'variance \eqn{\sigma^2}, or "fixed" to use a fixed value (an empirical Bayes
#'estimate, for example).
#'@param Sigma2 A fixed value for the error variance \eqn{\sigma^2}. Not necessary
#'when method.sigma is equal to "Jeffreys". Use this argument to pass the (estimated)
#'value of Sigma2 in case "fixed" is selected for method.sigma. The default (Sigma2 = 1)
#'is not suitable for most purposes and should be replaced.
#'@param burn Number of burn-in MCMC samples. Default is 1000.
#'@param nmc Number of posterior draws to be saved. Default is 5000.
#'@param thin Thinning parameter of the chain. Default is 1 (no thinning).
#'@param alpha Level for the credible intervals. For example, alpha = 0.05 results in
#'95\% credible intervals.
#'
#'@return \item{BetaHat}{Posterior mean of Beta, a \eqn{p} by 1 vector.}
#' \item{LeftCI}{The left bounds of the credible intervals.}
#' \item{RightCI}{The right bounds of the credible intervals.}
#' \item{BetaMedian}{Posterior median of Beta, a \eqn{p} by 1 vector.}
#' \item{Sigma2Hat}{Posterior mean of error variance \eqn{\sigma^2}. If method.sigma =
#' "fixed" is used, this value will be equal to the user-selected value of Sigma2
#' passed to the function.}
#' \item{TauHat}{Posterior mean of global scale parameter tau, a positive scalar.
#' If method.tau = "fixed" is used, this value will be equal to the user-selected value
#' of tau passed to the function.}
#' \item{BetaSamples}{Posterior samples of Beta.}
#' \item{TauSamples}{Posterior samples of tau.}
#' \item{Sigma2Samples}{Posterior samples of Sigma2.}
#'
#' @seealso \code{\link{HS.normal.means}} for a faster version specifically for the sparse
#' normal means problem (design matrix X equal to identity matrix) and
#' \code{\link{HS.post.mean}} for a fast way to estimate the posterior mean in the sparse
#' normal means problem when a value for tau is available.
#'
#' @examples
#' \dontrun{#In this example, there are no relevant predictors
#' #20 observations, 30 predictors (betas)
#' y <- rnorm(20)
#' X <- matrix(rnorm(20*30) , 20)
#' res <- horseshoe(y, X, method.tau = "truncatedCauchy", method.sigma = "Jeffreys")
#'
#' plot(y, X%*%res$BetaHat) #plot predicted values against the observed data
#' res$TauHat #posterior mean of tau
#' HS.var.select(res, y, method = "intervals") #selected betas
#' #Ideally, none of the betas is selected (all zeros)
#' #Plot the credible intervals
#' library(Hmisc)
#' xYplot(Cbind(res$BetaHat, res$LeftCI, res$RightCI) ~ 1:30)
#'}
#'
#' \dontrun{ #The horseshoe applied to the sparse normal means problem
#' # (note that HS.normal.means is much faster in this case)
#' X <- diag(100)
#' beta <- c(rep(0, 80), rep(8, 20))
#' y <- beta + rnorm(100)
#' res2 <- horseshoe(y, X, method.tau = "truncatedCauchy", method.sigma = "Jeffreys")
#' #Plot predicted values against the observed data (signals in blue)
#' plot(y, X%*%res2$BetaHat, col = c(rep("black", 80), rep("blue", 20)))
#' res2$TauHat #posterior mean of tau
#' HS.var.select(res2, y, method = "intervals") #selected betas
#' #Ideally, the final 20 predictors are selected
#' #Plot the credible intervals
#' library(Hmisc)
#' xYplot(Cbind(res2$BetaHat, res2$LeftCI, res2$RightCI) ~ 1:100)
#'}
#'
#' @export

horseshoe = function(y,X, method.tau = c("fixed", "truncatedCauchy","halfCauchy"), tau = 1,
                     method.sigma = c("fixed", "Jeffreys"), Sigma2 = 1,
                     burn = 1000, nmc = 5000, thin = 1, alpha = 0.05)
{

  method.tau = match.arg(method.tau)

  method.sigma = match.arg(method.sigma)

  ptm=proc.time()
  N=burn+nmc
  effsamp=(N-burn)/thin
  n=nrow(X)
  p=ncol(X)

  ## parameters ##
  Beta=rep(0,p); lambda=rep(1,p);
  sigma_sq = Sigma2;

  ## output ##
  betaout=matrix(0,p,effsamp)
  tauout=rep(0,effsamp)
  sigmaSqout=rep(0,effsamp)

  ## which algo to use ##
  if(p>n)
    algo=1
  else
    algo=2
  ## matrices ##
  I_n=diag(n)
  l0=rep(0,p)
  l1=rep(1,n)
  l2=rep(1,p)
  if(algo==2)
  {
    Q_star=t(X)%*%X
  }
  ## start Gibb's sampling ##
  for(i in 1:N)
  {
    ## update beta ##
    if(algo==1)
    {
      lambda_star=tau*lambda
      U=as.numeric(lambda_star^2)*t(X)
      ## step 1 ##
      u=stats::rnorm(l2,l0,lambda_star)
      v=X%*%u + stats::rnorm(n)
      ## step 2 ##
      v_star=solve((X%*%U+I_n),((y/sqrt(sigma_sq))-v))
      Beta=sqrt(sigma_sq)*(u+U%*%v_star)
    }
    else if(algo==2)
    {
      lambda_star=tau*lambda
      # Q=(1/sigma_sq)*(t(X)%*%X+diag(1/lambda_star^2,p,p))#
      # b=(t(y)*X)/sigma_sq #
      L=chol((1/sigma_sq)*(Q_star+diag(1/as.numeric(lambda_star^2),p,p)))
      v=solve(t(L),t(t(y)%*%X)/sigma_sq)
      mu=solve(L,v)
      u=solve(L,stats::rnorm(p))
      Beta=mu+u
    }


    ## update lambda_j's in a block using slice sampling ##
    eta = 1/(lambda^2)
    upsi = stats::runif(p,0,1/(1+eta))
    tempps = Beta^2/(2*sigma_sq*tau^2)
    ub = (1-upsi)/upsi
    # now sample eta from exp(tempv) truncated between 0 & upsi/(1-upsi)
    Fub = 1 - exp(-tempps*ub) # exp cdf at ub
    Fub[Fub < (1e-4)] = 1e-4;  # for numerical stability
    up = stats::runif(p,0,Fub)
    eta = -log(1-up)/tempps
    lambda = 1/sqrt(eta);

    ## update tau ##
    ## Only if prior on tau is used
    if(method.tau == "halfCauchy"){
      tempt = sum((Beta/lambda)^2)/(2*sigma_sq)
      et = 1/tau^2
      utau = stats::runif(1,0,1/(1+et))
      ubt = (1-utau)/utau
      Fubt = stats::pgamma(ubt,(p+1)/2,scale=1/tempt)
      Fubt = max(Fubt,1e-8) # for numerical stability
      ut = stats::runif(1,0,Fubt)
      et = stats::qgamma(ut,(p+1)/2,scale=1/tempt)
      tau = 1/sqrt(et)
    }#end if

    if(method.tau == "truncatedCauchy"){
      tempt = sum((Beta/lambda)^2)/(2*sigma_sq)
      et = 1/tau^2
      utau = stats::runif(1,0,1/(1+et))
      ubt_1=1
      ubt_2 = min((1-utau)/utau,p^2)
      Fubt_1 = stats::pgamma(ubt_1,(p+1)/2,scale=1/tempt)
      Fubt_2 = stats::pgamma(ubt_2,(p+1)/2,scale=1/tempt)
      #Fubt = max(Fubt,1e-8) # for numerical stability
      ut = stats::runif(1,Fubt_1,Fubt_2)
      et = stats::qgamma(ut,(p+1)/2,scale=1/tempt)
      tau = 1/sqrt(et)
    }

    ## update sigma_sq ##
    if(method.sigma == "Jeffreys"){
      if(algo==1)
      {
        E_1=max(t(y-X%*%Beta)%*%(y-X%*%Beta),(1e-10))
        E_2=max(sum(Beta^2/((tau*lambda))^2),(1e-10))
      }

      else
      {
        E_1=max(t(y-X%*%Beta)%*%(y-X%*%Beta),1e-8)
        E_2=max(sum(Beta^2/((tau*lambda))^2),1e-8)
      }
      sigma_sq=1/stats::rgamma(1,(n+p)/2,scale=2/(E_1+E_2))
    }



    if (i%%1000 == 0)
    {
      print(i)
    }

    if(i > burn && i%%thin== 0)
    {
      betaout[,(i-burn)/thin] = Beta
      tauout[(i-burn)/thin]=tau
      sigmaSqout[(i-burn)/thin]=sigma_sq
    }
  }
  pMean=apply(betaout,1,mean)
  pMedian=apply(betaout,1,stats::median)
  pSigma=mean(sigmaSqout)
  pTau=mean(tauout)
  #t=proc.time()-ptm

  #construct credible sets
  left <- floor(alpha*effsamp/2)
  right <- ceiling((1-alpha/2)*effsamp)

  BetaSort <- apply(betaout, 1, sort, decreasing = F)
  left.points <- BetaSort[left, ]
  right.points <- BetaSort[right, ]

  result=list("BetaHat"=pMean, "LeftCI" = left.points,
              "RightCI" = right.points,"BetaMedian"=pMedian,
              "Sigma2Hat"=pSigma,"TauHat"=pTau,"BetaSamples"=betaout,
              "TauSamples" = tauout, "Sigma2Samples" = sigmaSqout)
  return(result)
}

