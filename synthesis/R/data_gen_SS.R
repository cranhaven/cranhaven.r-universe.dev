#' Linear Gaussian state-space model
#'
#' @description
#' Generates data from a specific linear Gaussian state space model of the form
#' \eqn{ x_{t} = \phi x_{t-1} + \sigma_v v_t } and \eqn{ y_t = x_t +
#' \sigma_e e_t }, where \eqn{v_t} and \eqn{e_t} denote independent standard
#' Gaussian random variables, i.e. \eqn{N(0,1)}.
#'
#' @param theta The parameters \eqn{\theta=\{\phi,\sigma_v,\sigma_e\}} of the LGSS model.
#' @param nobs  The data length to be generated.
#' @param start A numeric value indicating the starting value for the time series.
#' If the starting point is not specified, it is generated randomly.
#' @param do.plot Logical value. If TRUE (default value), a plot of the generated LGSS system is shown.
#'
#' @return A list of two variables, state and response.
#' @export
#' @importFrom graphics legend
#'
#' @references #Dahlin, J. & Schon, T. B. 'Getting Started with Particle Metropolis-Hastings for Inference in Nonlinear Dynamical Models.'
#' Journal of Statistical Software, Code Snippets, 88(2): 1--41, 2019.
#'
#' @examples
#' data.LGSS <- data.gen.LGSS(theta=c(0.75,1.00,0.10), nobs=500, start=0)

data.gen.LGSS <- function(theta, nobs, start = runif(n = 1, min = -1, max = 1), do.plot = TRUE) {
    phi <- theta[1]
    sigmav <- theta[2]
    sigmae <- theta[3]

    state <- matrix(0, nrow = nobs + 1, ncol = 1)
    obs <- matrix(0, nrow = nobs + 1, ncol = 1)

    state[1] <- start
    obs[1] <- NA

    for (t in 2:(nobs + 1)) {
        state[t] <- phi * state[t - 1] + sigmav * rnorm(1)
        obs[t] <- state[t] + sigmae * rnorm(1)
    }

    # plotting
    if (do.plot) {
        title <- paste("Linear Gaussian state-space model")
        # plot(1:nobs, x, xlab = 'n', ylab = 'x[n]', main = title, type = 'l')
        plot.ts(cbind(state, obs), main = title, plot.type = c("multiple", "single")[2],
                xlab = NA, ylab=NA, col=c("black","red"))
        legend("topright", legend = c("state","obs"), lty=c(1,1), col=c("red","black"),bty = "n")
    }

    list(x = state, y = obs)
}

#' Stochastic Volatility model
