#'Simulates synthetic data from the ETAS model
#'

#' This function simulates sample data from the ETAS model over a particular interval [0,T]. 

#'The Epidemic Type Aftershock Sequence (ETAS) model is widely used to quantify the degree of seismic activity in a geographical region, and to forecast the occurrence of future mainshocks and aftershocks (Ross 2016). The temporal ETAS model is a point process where the probability of an earthquake occurring at time \eqn{t} depends on the previous seismicity \eqn{H_t}{Ht}, and is defined by the conditional intensity function:

#' \deqn{ \lambda(t|H_t) = \mu + \sum_{t[i] < t}  \kappa(m[i]|K,\alpha) h(t[i]|c,p)}{ \lambda(t|Ht) = \mu + \sum \kappa(m[i]|K,\alpha) h(t[i]|c,p)}

#' where

#' \deqn{\kappa(m_i|K,\alpha) = Ke^{\alpha \left( m_i-M_0 \right)}}{\kappa(m[i]|K,\alpha) = K * exp(\alpha(m[i]-M0))}

#' and

#' \deqn{ h(t_i|c,p) = \frac{(p-1)c^{p-1}}{(t-t_i+c)^p}}{ h(t[i]|c,p) = (p-1) * c^(p-1) * (t-t[i]+c)^(-p)}

#' where the summation is over all previous earthquakes that occurred in the region, with the i'th such earthquake occurring at time \eqn{ t_i}{t[i]} and having magnitude \eqn{ m_i}{m[i]}. The quantity \eqn{M_0}{M0} denotes the magnitude of completeness of the catalog, so that \eqn{m_i \geq  M_0}{m[i] \ge M0} for all i. The temporal ETAS model has 5 parameters: \eqn{\mu} controls the background rate of seismicity, \eqn{ K} and \eqn{ \alpha} determine the productivity (average number of aftershocks) of an earthquake with magnitude \eqn{m}, and \eqn{c} and \eqn{ p}  are the parameters of the Modified Omori Law (which has here been normalized to integrate to 1) and represent the speed at which the aftershock rate decays over time. Each earthquake is assumed to have a magnitude which is an independent draw from the Gutenberg-Richter law \eqn{ p(m_i) = \beta e^{\beta(m_i-M_0)}}{ p(m) = \beta * exp(\beta(m-M0)}.

#' \deqn{}

#' This function simulates sample data from the ETAS model over a particular interval [0,T].

#' @param mu Parameter of the ETAS model as described above.
#' @param K Parameter of the ETAS model as described above.
#' @param alpha Parameter of the ETAS model as described above.
#' @param c Parameter of the ETAS model as described above.
#' @param p Parameter of the ETAS model as described above.
#' @param beta Parameter of the Gutenberg-Richter law used to generate earthquake magnitudes.
#' @param M0 Magnitude of completeness.
#' @param T Length of the time window [0,T] to simulate the catalog over.
#' @param displayOutput If TRUE then prints the number of earthquakes simulated so far.
#' @return A list consisting of
#'   \item{ts}{The simulated earthquake times}
#'   \item{magnitudes}{The simulated earthquake magnitudes}
#'   \item{branching}{The simulated branching structure, where branching[i] is the index of the earthquake that triggered earthquake i, or 0 if earthquake i is a background event}
#' @examples
#'\dontrun{
#' beta <- 2.4; M0 <- 3
#' simulateETAS(0.2, 0.2, 1.5, 0.5, 2, beta, M0, T=500, displayOutput=FALSE)
#'}
#' @author Gordon J Ross
#' @references Gordon J. Ross - Bayesian Estimation of the ETAS Model for Earthquake Occurrences (2016)
#' @export
simulateETAS <- function(mu,K,alpha,c,p,beta,M0,T,displayOutput=TRUE) {
  maxTime <- T; rm(T)
  cp <-c; rm(c)
  if (beta <= alpha || K*beta / (beta-alpha) >= 1) {
    print("warning: parameter choices will lead to supercriticality if T is large enough. Infinite events may be generated")
  }
  
  h <- function(z) {(p-1)*cp^(p-1) * 1/( (z+cp)^p )}
  kappa  <- function(m) {
    res <- length(m); res[m<M0] <- 0; inds <- which(m>=M0)
    if (length(inds)>0) {res[inds] <- K * exp(alpha*(m[inds]-M0));}
    res
  }
  
  maxh <- max(h(seq(0,maxTime,length=10000)))
  rmarks <- function(n) {rexp(n,beta) + M0} 
  
  x <- simulateNHPP(function(t) {mu} ,mu,T=maxTime)
  m <- rmarks(length(x))
  
  count <- 1
  caused <- rep(0,length(x))
  
  while(TRUE) { #add new events onto x...
    if (displayOutput==TRUE) {
      print(sprintf("%s events generated so far",count,length(x)))
    }

    if (count > length(x)) {break}
    pt <- x[count] 
    
    fn <- function(t) {kappa(m[count])*h(t)}
    maxintensity <- kappa(m[count])*maxh
    
    pts <- simulateNHPP(fn, maxintensity=maxintensity, T=maxTime-pt)
    if (!is.null(pts)) {
      x <- c(x, pts+pt) 
      m <- c(m,rmarks(length(pts)))
      caused <- c(caused, rep(pt,length(pts)))
    } 
    count <- count+1
  }
  
  ord <- order(x)
  x <- x[ord]; m <- m[ord]; caused <- caused[ord]
  for (i in 1:length(caused)) {
    if (caused[i]==0) {next}
    caused[i] <- which(x==caused[i])
  }
  
  return(list(ts=x,magnitudes=m,branching=caused))
}