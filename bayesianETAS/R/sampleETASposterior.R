#' Draws samples from the posterior distribution of the ETAS model
#'

#' This function implements the latent variable MCMC scheme from (Ross 2016) which draws samples from the Bayesian posterior distribution of the Epidemic Type Aftershock Sequence (ETAS)  model.

#' \deqn{}

#'The ETAS model is widely used to quantify the degree of seismic activity in a geographical region, and to forecast the occurrence of future mainshocks and aftershocks (Ross 2016). The temporal ETAS model is a point process where the probability of an earthquake occurring at time \eqn{t} depends on the previous seismicity \eqn{H_t}{Ht}, and is defined by the conditional intensity function:

#' \deqn{ \lambda(t|H_t) = \mu + \sum_{t[i] < t}  \kappa(m[i]|K,\alpha) h(t[i]|c,p)}{ \lambda(t|Ht) = \mu + \sum \kappa(m[i]|K,\alpha) h(t[i]|c,p)}

#' where

#' \deqn{\kappa(m_i|K,\alpha) = Ke^{\alpha \left( m_i-M_0 \right)}}{\kappa(m[i]|K,\alpha) = K * exp(\alpha(m[i]-M0))}

#' and

#' \deqn{ h(t_i|c,p) = \frac{(p-1)c^{p-1}}{(t-t_i+c)^p}}{ h(t[i]|c,p) = (p-1) * c^(p-1) * (t-t[i]+c)^(-p)}

#' where the summation is over all previous earthquakes that occurred in the region, with the i'th such earthquake occurring at time \eqn{ t_i}{t[i]} and having magnitude \eqn{ m_i}{m[i]}. The quantity \eqn{M_0}{M0} denotes the magnitude of completeness of the catalog, so that \eqn{m_i \geq  M_0}{m[i] \ge M0} for all i. The temporal ETAS model has 5 parameters: \eqn{\mu} controls the background rate of seismicity, \eqn{ K} and \eqn{ \alpha} determine the productivity (average number of aftershocks) of an earthquake with magnitude \eqn{m}, and \eqn{c} and \eqn{ p}  are the parameters of the Modified Omori Law (which has here been normalized to integrate to 1) and represent the speed at which the aftershock rate decays over time. Each earthquake is assumed to have a magnitude which is an independent draw from the Gutenberg-Richter law \eqn{ p(m_i) = \beta e^{\beta(m_i-M_0)}}{ p(m) = \beta * exp(\beta(m-M0)}.

#' @param ts Vector containing the earthquake times
#' @param magnitudes Vector containing the earthquake magnitudes
#' @param M0 Magnitude of completeness.
#' @param T Length of the time window [0,T] the catalog was observed over. If not specified, will be taken as the time of the last earthquake.
#' @param initval Initial value at which to start the estimation. If specified, should be a vector, with elements (mu, K, alpha, c, p). If unspecified, the sampler will be initialized at the maximum likelihood estimate of the model parameters
#' @param approx If TRUE then will approximate the true posterior using the infinite time approximation discussed in (Ross 2016)
#' @param sims Number of posterior samples to draw
#' @param burnin Number of burnin samples
#' @return A matrix containing the posterior samples. Each row is a single sample, and the columns correspond to (mu, K, alpha, c, p)

#' @examples
#'\dontrun{
#' beta <- 2.4; M0 <- 3; T <- 500
#' catalog <- simulateETAS(0.2, 0.2, 1.5, 0.5, 2, beta, M0, T)
#' sampleETASposterior(catalog$ts, catalog$magnitudes, M0, T, sims=5000)
#'}
#' @author Gordon J Ross
#' @references Gordon J. Ross - Bayesian Estimation of the ETAS Model for Earthquake Occurrences (2016)
#' @export
sampleETASposterior <- function(ts, magnitudes, M0, T=NA, initval=NA, approx=FALSE, sims=5000, burnin=500 ) {
  if (is.na(T)) {T <- max(ts)}
  numMCMCSamples <- 500
  
  marks <- magnitudes; maxTime <- T
  if (is.na(initval)) {
    print("No initial value passed. Computing maximum likelihood estimate...")
    n <- length(ts); maxTime2 <- maxTime
    if (n > 500) {
      n <- 500; maxTime2 <- ts[500]
    }
    initval <- maxLikelihoodETAS(ts[1:n],marks[1:n],M0,maxTime2, displayOutput=FALSE)$params
    print("Maximum likelihood estimate is:")
    print(initval)
    print("Will use this for initial value")
  }
  
  sims <- sims+burnin
  branching <- rep(0,length(ts)); mu <- initval[1]; K <- initval[2]; alpha <- initval[3]; cp <- initval[4]; p <- initval[5]
  
  print(sprintf("Starting Gibbs sampler. Will draw %s posterior samples...",sims))
  res <- .C("estimateETASBranchingC", ts=as.double(ts), marks=as.double(marks), branching=as.integer(branching), n=as.integer(length(ts)), maxTime = as.double(maxTime), M0=as.double(M0), sims=as.integer(sims), numMCMCSamples=as.integer(numMCMCSamples), approx=as.integer(approx), mu=as.double(mu), logK=as.double(log(K)), alpha=as.double(alpha), c=as.double(cp), p=as.double(p), mus=as.double(numeric(sims)), logKs=as.double(numeric(sims)), alphas=as.double(numeric(sims)), cs=as.double(numeric(sims)), ps=as.double(numeric(sims)),PACKAGE="bayesianETAS");
  
  bayesbranching <- cbind(res$mus,res$logKs,res$alphas, res$cs, res$ps)
  bayesbranching  <- bayesbranching[-c(1:burnin),]
  bayesbranching[,2] <- exp(bayesbranching[,2])
  
  return(bayesbranching)
}

