#' Automated Heuristic Search of a Proposal Standard Deviation for \code{bayes.2S} (sequential processing)
#'
#' The \link{bayes.2S} Gibbs sampler uses a Metropolis step for sampling the
#' incidence model parameters and requires specifying a standard deviation for
#' the normal proposal (jumping) distribution. This function uses a heuristic
#' algorithm to find a proposal distribution standard deviation such that the
#' Metropolis sampler accepts proposed draws at an acceptance rate within the
#' user-defined interval (by default around 20--25%). This is the sequential processing analogue to \link{search.prop.sd} which does parallel processing by default.
#'
#' @details
#' Starting from an initial \code{bayes.2S} model object \code{m}, the function
#' attempts to calibrate the standard deviation of the proposal distribution.
#' Specifically, it:
#' \enumerate{
#'   \item Runs an initial update of \code{ndraws} iterations and computes an
#'     acceptance rate.
#'   \item If the acceptance rate lies within \code{acc.bounds.X}, the number
#'     of MCMC draws \code{ndraws} is doubled, and the process repeats.
#'   \item Otherwise, the proposal standard deviation \eqn{\sigma} is adjusted
#'     based on whether the acceptance rate \eqn{p} is below the lower bound \eqn{a}
#'     or above the upper bound \eqn{b} of \code{acc.bounds.X}.
#'   \item The formula for adjustment is:
#'     \deqn{\sigma \leftarrow \sigma \times (1 - \frac{ (a-p)}{a}) \quad\text{if } p < a, \quad
#'           \sigma \leftarrow \sigma \times (1 + \frac{ (p-b)}{b}) \quad\text{if } p > b.}
#' }
#' By default, if the acceptance rate falls within \eqn{[0.2, 0.25]}, that \eqn{\sigma}
#' is considered acceptable, and the process continues until \code{succ.min} consecutive
#' successes (doubles) are achieved. 
#'
#' @param m A model object of class \code{bayes.2S}.
#' @param ndraws Starting number of MCMC iterations after which the acceptance rate
#'   is first evaluated. Defaults to 1000.
#' @param succ.min The algorithm doubles the number of MCMC draws \code{succ.min} times
#'   (each time the acceptance rate is within \code{acc.bounds.X}), ensuring
#'   stability. Defaults to 3.
#' @param acc.bounds.X A numeric vector of length two specifying the lower and upper
#'   bounds for the acceptable acceptance rate. Defaults to \code{c(0.2, 0.25)}.
#'
#' @return A \code{list} with the following elements:
#' \describe{
#'   \item{\code{prop.sd.X}}{The final (adjusted) proposal standard deviation.}
#'   \item{\code{ac.X}}{The acceptance rate in the last iteration.}
#' }
#'
#' @examples
#' \donttest{
#' # Generate data according to Klausch et al. (2025) PIM
#' set.seed(2025)
#' dat = gen.dat(kappa = 0.7, n = 1e3, theta = 0.2,
#'               p = 1, p.discrete = 1,
#'               beta.X = c(0.2, 0.2), beta.W = c(0.2, 0.2),
#'               v.min = 20, v.max = 30, mean.rc = 80,
#'               sigma.X = 0.2, mu.X = 5, dist.X = "weibull",
#'               prob.r  = 1)
#'
#' # An initial model fit with a moderate number of ndraws (e.g., 1e3)
#' mod = bayes.2S(
#'   Vobs = dat$Vobs, Z.X = dat$Z, Z.W = dat$Z, r = dat$r,
#'   kappa = 0.7, update.kappa = FALSE, ndraws = 1e3, chains = 2,
#'   prop.sd.X = 0.005, parallel = TRUE, dist.X = "weibull"
#' )
#'
#' # Running the automated search
#' search.sd <- search.prop.sd_seq(m = mod)
#' print(search.sd)
#' }
#'
#' @export
search.prop.sd_seq = function(m, ndraws = 1000, succ.min = 3, acc.bounds.X =c(0.2,0.25)){
  found.X = found.S = FALSE
  it = 1; succ = 0;
  while(succ!=succ.min){
    message(paste('Iteration',it,'\n'))
    if(it == 1) { ac.X.cur = mean(m$ac.X)
    prop.sd.X = m$prop.sd.X}
    if(it >1) {
      sink(file = "NUL", type = "output")
      m = bayes.2S_seq( prev.run = m, ndraws.update = ndraws, prop.sd.X = prop.sd.X)
      sink(type = "output")
      ac.X.cur = mean(m$ac.X.cur)
    }
    acc.bounds.mean = (acc.bounds.X[2]-acc.bounds.X[1])/2 + acc.bounds.X[1]
    ac.X = mean(m$ac.X)
    message( paste('Acceptance rate was:', round(ac.X.cur, 3),'\n' ))
    ac.X = ac.X.cur
    if((ac.X > acc.bounds.X[1] & ac.X < acc.bounds.X[2]) ){
      found.X = TRUE} else{
        if(ac.X < acc.bounds.X[1]) {
          dif =  1-(acc.bounds.X[1] - ac.X)/acc.bounds.X[1]
          prop.sd.X=prop.sd.X * dif
        }
        if(ac.X > acc.bounds.X[2]) {
          dif =  1+(ac.X - acc.bounds.X[2])/acc.bounds.X[2]
          prop.sd.X=prop.sd.X * dif
        }
        found.X = FALSE
        message(paste('prop.sd.X is set to', round(prop.sd.X,3),'\n'))
      }
    it=it+1
    if(found.X ){
      ndraws = ndraws*2
      succ = succ +1
      found.X = FALSE
      if(succ!=succ.min) message(paste('Success. Doubling number of MCMC draws:',ndraws,'\n'))
    }
  }
  message('Finished calibrating proposal variance. \n')
  ret= list()
  ret$prop.sd.X = prop.sd.X
  ret$ac.X      = ac.X
  #ret$mod       = m
  ret
}
