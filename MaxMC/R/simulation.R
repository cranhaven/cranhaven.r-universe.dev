#' Maximized Monte Carlo Simulation
#'
#' Generates N Monte Carlo replicates of a statistic for given nuisance parameter value.
#'
#' @param v A vector parameters. The vector \code{v} is use to
#'  specify the \code{dgp}. Note that if \code{dgp} is a
#'  function  of only \code{y} then we do not need to specify
#'  \code{v}. Default value is NULL.
#'
#' @inheritParams mmc
#'
#' @return The vector of replication of test statistic.
#'
#' @keywords internal
#'
#' @example /inst/examples/simulation_mmc_example.R
#'
simulation_mmc <- function(y, statistic, dgp = function(y, v) sample(y, replace = TRUE),
                           v, N = 99, ...) {
    stat <- function(y, v, ...) {
      # Generate new observation y
      ran.y <- dgp(y,v)
      # Compute the statistic on this new observation y
      statistic(ran.y, ...)
    }

    S <- replicate(N, stat(y, v), ...)
    return(S)
}

#' Monte Carlo Simulation
#'
#' Generates N Monte Carlo replicates of a statistic.
#'
#' @inheritParams mc
#' @return The vector of replication of test statistic.
#'
#' @keywords internal
#'
#' @example /inst/examples/simulation_mc_example.R
#'
#'
simulation_mc <- function(y, statistic, dgp = function(y) sample(y, replace = TRUE),
                          N = 99, ...) {
    stat <- function(y, ...) {
        # Generate new observation y
        ran.y <- dgp(y)
        # Compute the statistic on this new observation y
        statistic(ran.y, ...)
    }

    S <- replicate(N, stat(y), ...)
    return(S)
}
