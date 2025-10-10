
#' The horseshoe prior for the sparse normal means problem
#'
#'  Apply the horseshoe prior to the normal means problem
#'  (i.e. linear regression with the design matrix equal to the identity matrix).
#'  Computes the posterior mean, median and credible intervals. There are options for
#'  empirical Bayes (estimate of tau and or Sigma2 plugged in) and full Bayes (truncated
#'  or non-truncated half-Cauchy on tau, Jeffrey's prior on Sigma2). For the full Bayes
#'  version, the truncated half-Cauchy prior is recommended by Van der Pas et al. (2016).
#'
#' The normal means model is:
#' \deqn{y_i=\beta_i+\epsilon_i, \epsilon_i \sim N(0,\sigma^2)}
#'
#' And the horseshoe prior:
#' \deqn{\beta_j \sim N(0,\sigma^2 \lambda_j^2 \tau^2)}
#' \deqn{\lambda_j \sim Half-Cauchy(0,1).}
#'
#' Estimates of \eqn{\tau} and \eqn{\sigma^2} may be plugged in (empirical Bayes), or those
#' parameters are equipped with hyperpriors (full Bayes).
#'
#' @param y The data. A \eqn{n*1} vector.
#' @param method.tau Method for handling \eqn{\tau}. Select "fixed" to plug in an
#' estimate of tau (empirical Bayes), "truncatedCauchy" for the half-
#' Cauchy prior truncated to [1/n, 1], or "halfCauchy" for a
#' non-truncated half-Cauchy prior. The truncated Cauchy prior is recommended over
#' the non-truncated version.
#' @param tau Use this argument to pass the (estimated) value of \eqn{\tau} in case "fixed"
#' is selected for method.tau. Not necessary when method.tau is equal to"halfCauchy" or
#' "truncatedCauchy". The function \code{\link{HS.MMLE}} can be used to compute an
#' estimate of tau. The default (tau = 1) is not suitable for most purposes and should
#' be replaced.
#' @param method.sigma Select "fixed" for a fixed error variance, or "Jeffreys"
#' to use Jeffrey's prior.
#' @param Sigma2 The variance of the data - only necessary when "fixed" is
#' selected for method.sigma. The default (Sigma2 = 1) is not suitable for
#' most purposes and should be replaced.
#' @param burn Number of samples used for burn-in. Default is 1000.
#' @param nmc Number of MCMC samples taken after burn-in. Default is 5000.
#' @param alpha The level for the credible intervals. E.g. alpha = 0.05 yields
#' 95\% credible intervals
#' @return \item{BetaHat}{The posterior mean (horseshoe estimator) for each of the datapoints.}
#' \item{LeftCI}{The left bounds of the credible intervals.}
#' \item{RightCI}{The right bounds of the credible intervals.}
#' \item{BetaMedian}{Posterior median of Beta, a \eqn{n} by 1 vector.}
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
#'
#' @references
#' van der Pas, S.L., Szabo, B., and van der Vaart, A. (2017), Uncertainty
#' quantification for the horseshoe (with discussion). Bayesian Analysis
#' 12(4), 1221-1274.
#'
#' van der Pas, S.L., Szabo, B., and van der Vaart A. (2017), Adaptive
#' posterior contraction rates for the horseshoe. Electronic Journal of
#' Statistics 10(1), 3196-3225.
#'
#' @seealso \code{\link{HS.post.mean}} for a fast way to compute the posterior mean
#' if an estimate of tau is available. \code{\link{horseshoe}} for linear regression.
#' \code{\link{HS.var.select}} to perform variable selection.
#'
#' @examples #Empirical Bayes example with 20 signals, rest is noise
#' #Posterior mean for the signals is plotted
#' #And variable selection is performed using the credible intervals
#' #And the credible intervals are plotted
#' truth <- c(rep(0, 80), rep(8, 20))
#' data <-  truth + rnorm(100, 1)
#' tau.hat <- HS.MMLE(data, Sigma2 = 1)
#' res.HS1 <- HS.normal.means(data, method.tau = "fixed", tau = tau.hat,
#' method.sigma = "fixed", Sigma2 = 1)
#' #Plot the posterior mean against the data (signals in blue)
#' plot(data, res.HS1$BetaHat, col = c(rep("black", 80), rep("blue", 20)))
#' #Find the selected betas (ideally, the last 20 are equal to 1)
#' HS.var.select(res.HS1, data, method = "intervals")
#' #Plot the credible intervals
#' library(Hmisc)
#' xYplot(Cbind(res.HS1$BetaHat, res.HS1$LeftCI, res.HS1$RightCI) ~ 1:100)
#'
#'
#' #Full Bayes example with 20 signals, rest is noise
#' #Posterior mean for the signals is plotted
#' #And variable selection is performed using the credible intervals
#' #And the credible intervals are plotted
#' truth <- c(rep(0, 80), rep(8, 20))
#' data <-  truth + rnorm(100, 3)
#' res.HS2 <- HS.normal.means(data, method.tau = "truncatedCauchy", method.sigma = "Jeffreys")
#' #Plot the posterior mean against the data (signals in blue)
#' plot(data, res.HS2$BetaHat, col = c(rep("black", 80), rep("blue", 20)))
#' #Find the selected betas (ideally, the last 20 are equal to 1)
#' HS.var.select(res.HS2, data, method = "intervals")
#' #Plot the credible intervals
#' library(Hmisc)
#' xYplot(Cbind(res.HS2$BetaHat, res.HS2$LeftCI, res.HS2$RightCI) ~ 1:100)
#' @export

HS.normal.means <- function(y, method.tau = c("fixed", "truncatedCauchy", "halfCauchy"),
                                      tau = 1, method.sigma = c("fixed", "Jeffreys"), Sigma2 = 1,
                                      burn = 1000, nmc = 5000, alpha = 0.05)
{

  method.tau = match.arg(method.tau)

  method.sigma = match.arg(method.sigma)

  n <- length(y)

  BetaSave = matrix(0, nmc, n)
  TauSave = rep(0, nmc)
  Sigma2Save = rep(0, nmc)

  #Initialize
  Lambda2 = rep(1, n)
  nu = rep(1, n)
  xi = 1
  Beta = y
  Tau = tau

  if(method.sigma == "Jeffreys"){
    Sigma2 = 0.95*stats::var(y)
    }

  if(method.tau != "fixed"){
    Tau = max(sum( abs(y) >= sqrt(2*Sigma2*log(n)) )/n, 1/n)
    #use the simple estimator to initialize tau
  }


  #Initialize
  Lambda = 1/abs(y)^2
  Tau = tau

  for(t in 1:(nmc+burn)){

    if (t%%1000 == 0){print(t)}

    #Block-update beta
    a = (Tau^2)*(Lambda^2)
    s = sqrt(Sigma2*a/(1+a))
    m = (a/(1+a))*y
    Beta = stats::rnorm(n, m, s)
    Theta = Beta/(Lambda)

    if(method.tau == "halfCauchy"){
      # Now update Tau^2 via parameter expansion
      G = 1/sqrt(stats::rgamma(1,(n+1)/2, rate = (1+sum(Theta^2))/2))
      Z = y/(Theta*Lambda)
      a = (Lambda*Theta)^2
      b = sum(a)
      s2 = 1/(1+b)
      m = {s2}*sum(a*Z)
      Delta = stats::rnorm(1,m,sqrt(s2))
      Tau = abs(Delta)*G
    }

    if(method.tau == "truncatedCauchy"){
      #Metropolis-Hasting with Gamma proposal
      #Truncation after gamma proposal
      tempt = sum(Beta^2/Lambda^2)/(2*Sigma2)
      et = 1/Tau^2
      utau = stats::runif(1,0,1/(1+et))
      ubt_1=1
      ubt_2 = min((1-utau)/utau,n^2)
      Fubt_1 = stats::pgamma(ubt_1,(n+1)/2,scale=1/tempt)
      Fubt_2 = stats::pgamma(ubt_2,(n+1)/2,scale=1/tempt)
      #Fubt = max(Fubt,1e-8) # for numerical stability
      ut = stats::runif(1,Fubt_1,Fubt_2)
      et = stats::qgamma(ut,(n+1)/2,scale=1/tempt)
      Tau = 1/sqrt(et)
    }

    # Now update Sigma2
    # Jeffreys prior is assumed

    if(method.sigma == "Jeffreys"){
      Sigma2 = 1/stats::rgamma(1, n, rate = sum( (y-Beta)^2 )/2 + sum( Beta^2/(Tau^2*Lambda^2) )/2)
    }

    #Update lambda
    Z = y/Theta
    V2 = 1/stats::rgamma(n, 1, rate = (Lambda^2+1)/2)
    num1 = V2*Theta^2
    den = 1 + num1
    s = sqrt(V2/den)
    m = (num1/den)*Z
    Lambda = stats::rnorm(n, m, s)

    #Save results
    if(t > burn){
      BetaSave[t-burn, ] = Beta
      TauSave[t-burn] = Tau
      Sigma2Save[t-burn] = Sigma2
    }

  }#end MCMC

  #Compute results
  BetaHat = colMeans(BetaSave)
  BetaMedian = apply(BetaSave, 2, stats::median)
  TauHat = mean(TauSave)
  Sigma2Hat = mean(Sigma2Save)

  #Construct marginal credible sets
  left <- floor(alpha*nmc/2)
  right <- ceiling((1-alpha/2)*nmc)

  BetaSort <- apply(BetaSave, 2, sort, decreasing = F)
  left.points <- BetaSort[left, ]
  right.points <- BetaSort[right, ]

  result <- list("BetaHat" = BetaHat, "LeftCI" = left.points,
                 "RightCI" = right.points, "BetaMedian" = BetaMedian,
                 "Sigma2Hat" = Sigma2Hat,
                 "TauHat" = TauHat, "BetaSamples" = BetaSave,
                 "TauSamples" = TauSave, "Sigma2Samples" = Sigma2Save )

  return(result)
}
