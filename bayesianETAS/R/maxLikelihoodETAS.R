#' Estimate the parameters of the ETAS model using maximum likelihood.
#'

#'The Epidemic Type Aftershock Sequence (ETAS) model is widely used to quantify the degree of seismic activity in a geographical region, and to forecast the occurrence of future mainshocks and aftershocks (Ross 2016). The temporal ETAS model is a point process where the probability of an earthquake occurring at time \eqn{t} depends on the previous seismicity \eqn{H_t}{Ht}, and is defined by the conditional intensity function:

#' \deqn{ \lambda(t|H_t) = \mu + \sum_{t[i] < t}  \kappa(m[i]|K,\alpha) h(t[i]|c,p)}{ \lambda(t|Ht) = \mu + \sum \kappa(m[i]|K,\alpha) h(t[i]|c,p)}

#' where

#' \deqn{\kappa(m_i|K,\alpha) = Ke^{\alpha \left( m_i-M_0 \right)}}{\kappa(m[i]|K,\alpha) = K * exp(\alpha(m[i]-M0))}

#' and

#' \deqn{ h(t_i|c,p) = \frac{(p-1)c^{p-1}}{(t-t_i+c)^p}}{ h(t[i]|c,p) = (p-1) * c^(p-1) * (t-t[i]+c)^(-p)}

#' where the summation is over all previous earthquakes that occurred in the region, with the i'th such earthquake occurring at time \eqn{ t_i}{t[i]} and having magnitude \eqn{ m_i}{m[i]}. The quantity \eqn{M_0}{M0} denotes the magnitude of completeness of the catalog, so that \eqn{m_i \geq  M_0}{m[i] \ge M0} for all i. The temporal ETAS model has 5 parameters: \eqn{\mu} controls the background rate of seismicity, \eqn{ K} and \eqn{ \alpha} determine the productivity (average number of aftershocks) of an earthquake with magnitude \eqn{m}, and \eqn{c} and \eqn{ p}  are the parameters of the Modified Omori Law (which has here been normalized to integrate to 1) and represent the speed at which the aftershock rate decays over time. Each earthquake is assumed to have a magnitude which is an independent draw from the Gutenberg-Richter law \eqn{ p(m_i) = \beta e^{\beta(m_i-M_0)}}{ p(m) = \beta * exp(\beta(m-M0)}.

#' \deqn{}

#' This function estimates the parameters of the ETAS model using maximum likelihood

#' @param ts Vector containing the earthquake times
#' @param magnitudes Vector containing the earthquake magnitudes
#' @param M0 Magnitude of completeness.
#' @param T Length of the time window [0,T] the catalog was observed over. If not specified, will be taken as the time of the last earthquake.
#' @param initval Initial value at which to start the estimation. A vector, with elements (mu, K, alpha, c, p)
#' @param displayOutput If TRUE then prints the out the likelihood during model fitting.
#' @return A list consisting of
#'   \item{params}{A vector containing the estimated parameters, in the order (mu,K,alpha,c,p,beta)}
#'   \item{loglik}{The corresponding loglikelihood}

#' @examples
#'\dontrun{
#' beta <- 2.4; M0 <- 3; T <- 500
#' catalog <- simulateETAS(0.2, 0.2, 1.5, 0.5, 2, beta, M0, T)
#' maxLikelihoodETAS(catalog$ts, catalog$magnitudes, M0, 500)
#'}
#' @author Gordon J Ross
#' @references Gordon J. Ross - Bayesian Estimation of the ETAS Model for Earthquake Occurrences (2016)
#' @export
maxLikelihoodETAS <- function(ts,magnitudes,M0,T,initval=NA,displayOutput=TRUE) {
  if (is.na(T)) {T <- max(ts)}
  marks <- magnitudes; maxTime <- T
  
  hawkesLikelihoodMarks <- function(ts, marks, cumint, mu, kappa, h, H, maxTime) {
    n <- length(ts)
    if (length(mu)==1) {mu <- rep(mu,n)}
    if (length(mu) != n) {print("mu wrong length"); return(NA)}
  
    temp <- log(mu[1]) # first obesrvation
    for (i in 2:n) {
      temp <- temp + log(mu[i] + sum(kappa(marks[1:(i-1)]) * h(ts[i] - ts[1:(i-1)])))
    }
    temp <- temp - cumint
    temp <- temp - sum(kappa(marks) * H(maxTime-ts))
    temp  
  }

  fn <- function(params) {
    mu <- params[1]; K <- params[2]; alpha <- params[3]; cp <- params[4]; p <- params[5]
    cumint <- mu*maxTime
    if (mu <= 0 || p <= 1 || alpha < 0 || K < 0) {return(Inf)} #need more than these
    h <- function(z) {(p-1)*cp^(p-1) * 1/( (z+cp)^p )}
    H <- function(z) {1- cp^(p-1)/(z+cp)^(p-1)}
    kappa  <- function(m) {
      res <- length(m); res[m<M0] <- 0; inds <- which(m>=M0)
      if (length(inds)>0) {res[inds] <- K * exp(alpha*(m[inds]-M0));}
      res
    }
    
    lik <- -hawkesLikelihoodMarks(ts, marks, cumint, mu, kappa, h, H, maxTime)
    if (displayOutput==TRUE) {print(c(mu,K,alpha,cp,p,lik))}
    lik
  }
  if (is.na(initval[1])) {
    initval <- c(length(ts)/maxTime,0.5,0.5,1,2)
  } else if (length(initval)==6) {
    initval <- initval[1:5]
  }
  
  temp <- optim(initval, fn, control=list(trace=FALSE))
  
  #now add the GR estimator....
  beta <- 1/mean(marks-M0)
  temp$par <- c(temp$par, beta)
  temp$value <- temp$value-sum(dexp(marks-M0,beta,log=TRUE))
  
  return(list(params=temp$par, loglik= -temp$value))
}
