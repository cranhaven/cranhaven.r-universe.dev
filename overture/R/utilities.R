#' Determine if a Metropolis–Hastings step should be accepted
#'
#' \code{AcceptProp} is a utility function to determine if a proposal should
#' be accepted in a Metropolis or Metropolis-Hastings step.
#'
#' The function uses the Metropolis choice for a Metropolis/Metropolis-Hastings
#' sampler, which accepts a proposed value \eqn{x'} with probability \deqn{
#' A(x', x) = min(1, P(x')/P(x) g(x|x')/g(x'|x)) } where \eqn{P(x)} is the
#' target distribution and \eqn{g(x'|x)} is the proposal distribution.
#'
#' @param log.curr log density of the target at the current value,
#'   \eqn{log(P(x))}
#' @param log.prop log density of the target at the proposed value,
#'   \eqn{log(P(x'))}
#' @param log.curr.to.prop log of transition distribution from current value to
#'   proposed value, \eqn{log(g(x'|x))}
#' @param log.prop.to.curr log of transition distribution from proposed value to
#'   current value, \eqn{log(g(x|x'))}
#' @return \code{TRUE/FALSE} for whether the proposal should be accepted or
#'   rejected, respectively
#' @example examples/example-AcceptProp.R
#' @export
AcceptProp <- function(log.curr, log.prop, log.curr.to.prop=0,
                           log.prop.to.curr=0) {
    u <- stats::runif(length(log.prop))
    log(u) <= (log.prop - log.curr + log.prop.to.curr - log.curr.to.prop)
}

DeltaNDefault <- function(n) {
    # Default proposal sd delta from Roberts & Rosenthal (2009)
    min(0.01, n^(-1/2))
}

#' Turn a non-adaptive Metropolis sampler into an adaptive Metropolis sampler
#'
#' Given a non-adaptive sampler of the form f(..., s), \code{Amwg} will return a
#' function g(...) that automatically adapts the Metropolis proposal standard
#' deviation s to try and achieve a target acceptance rate.
#'
#' \code{Amwg} uses the Adaptive Metropolis-Within-Gibbs algorithm from Roberts
#' & Rosenthal (2009), which re-scales the proposal standard deviation after a
#' fixed number of MCMC iterations have elapsed.  The goal of the algorithm is
#' to achieve a target acceptance rate for the Metropolis step.  After the
#' n\emph{th} batch of MCMC iterations the log of the proposal standard
#' deviation, \eqn{log(s)}, is increased/decreased by \eqn{\delta(n)}.
#' \eqn{log(s)} is increased by \eqn{\delta(n)} if the observed acceptance rate
#' is more than the target acceptance rate, or decreased by \eqn{\delta(n)} if
#' the observed acceptance rate is less than the target acceptance rate.
#' \code{Amwg} keeps track of the the acceptance rate by comparing the
#' previously sampled value from \code{f} to the next value.  If the two values
#' are equal, the proposal is considered to be rejected, whereas if the two
#' values are different the proposal is considered accepted.  \code{Amwg} will
#' optionally stop adapting the proposal standard deviation after
#' \code{stop.after} iterations.  Setting \code{stop.after} can be used, for
#' example, to stop adapting the proposal standard deviation after some burn-in
#' period.  If \code{stop.after=NA} (the default), \code{Amwg} will continue to
#' modify the proposal standard deviation throughout the entire MCMC.
#'
#' \code{DeltaN} is set to \eqn{\delta(n) = min(0.01, n^{-1/2})} unless
#' re-specified in the function call. Some care should be taken if re-specifying
#' \code{DeltaN}, as the ergodicity of the chain may not be preserved if certain
#' conditions aren't met.  See Roberts & Rosenthal (2009) in the references for
#' details.
#'
#' The proposal standard deviation \code{s} can be either a vector or a scalar.
#' If the initial value of \code{s} is a scalar, \eqn{f} will be treated as a
#' sampler for a scalar, a random vector, or a joint parameter update.
#' Alternatively, if the dimension of \eqn{s} is equal to the dimension of the
#' parameters returned by \eqn{f}, the individual elements \eqn{s} will be
#' treated as individual proposal standard deviations for the elements returned
#' by \eqn{f}.  This functionality can be used, for example, if \eqn{f} samples
#' each of its returned elements individually, updating each element using a
#' Metropolis step.  See the examples for an illustration of this use case.  In
#' such settings, \eqn{f} should be constructed to receive \eqn{s} as a vector
#' argument.
#'
#' @param f non-adaptive Metropolis sampler of the form f(..., s)
#' @param s initial value for the Metropolis proposal SD
#' @param batch.size number of iterations before proposal SD is adapted
#' @param target target acceptance rate
#' @param DeltaN function of the form f(n) which returns the adaptation amount
#'   based on the number of elapsed iterations, n.  Defaults to \eqn{\delta(n) =
#'   min(0.01, n^{-1/2})}
#' @param stop.after stop adapting proposal SD after this many iterations
#' @return Adaptive Metropolis sampler function of the form g(...).
#' @references  Gareth O. Roberts & Jeffrey S. Rosenthal (2009) Examples of
#'   Adaptive MCMC, Journal of Computational and Graphical Statistics, 18:2,
#'   349-367, \doi{10.1198/jcgs.2009.06134}
#'
#' @example examples/example-Amwg.R
#' @export
Amwg <- function(f, s, batch.size=50, target=0.44, DeltaN, stop.after=NA) {
    if(!IsPositiveInteger(batch.size)) {
        stop("'batch.size' must be an integer > 0")
    }
    if(!(IsPositiveInteger(stop.after) || is.na(stop.after))) {
        stop("'stop.after' must be an integer > 0")
    }
    if(!((target>0) && (target<1))) stop("'target' must be 0<target<1")

    if(is.na(stop.after)) stop.after <- Inf
    if(missing(DeltaN)) DeltaN <- DeltaNDefault
    n.iters <- 0
    n.accepted <- rep(0, length(s))
    accept.rate <- NA
    prev <- NA
    s <- s
    function(...) {
        if(n.iters==0) {
            prev <- f(..., s)
            if(!((length(s)==1) || (length(s)==length(prev)))) {
                stop("length(s) should be 1 or length(f(..., s))")
            }
        }

        ret <- f(..., s)
        n.iters <<- n.iters + 1
        if(length(s) > 1) { # Univariate updates for each component in ret
            n.accepted <<- n.accepted + (ret != prev)
        }
        else { # Scalar/random vector/joint update
            n.accepted <<- n.accepted + all(ret != prev)
        }
        accept.rate <<- n.accepted/n.iters
        prev <<- ret
        if((n.iters %% batch.size == 0) && (n.iters <= stop.after)){
            delta.n <- DeltaN(n.iters)
            s <<- ifelse(accept.rate > target,
                         s*exp(delta.n),
                         s*exp(-delta.n))
        }

        return(ret)
    }
}

