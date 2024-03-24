#' p-value Function
#'
#' Computes the p-value of the statistic by computing its rank
#' compared to its simulated values.
#'
#' We allow for four types of p-value: \code{leq}, \code{geq},
#' \code{absolute} and \code{two-tailed}. For one-tailed test,
#' \code{leq} returns the proportion of simulated values smaller
#' than the statistic while \code{geq} returns the proportion of
#' simulated values greater than the statistic. For two-tailed
#' test with a symmetric satistic, one can use the
#' absolute value of the statistic and its simulated values to
#' retrieve a two-tailed test (i.e. type = \code{absolute}).
#' If the statistic is not symmetric, one can specify the p-value
#' type as \code{two-tailed} which is equivalent to twice the minimum
#' of \code{leq} and \code{geq}.
#'
#' Ties in the ranking are broken according to a uniform
#' distribution.
#'
#' @param S0 An atomic vector. Value of the test statistic
#' applied to the data.
#' @param S A vector. It consists of replications of the test statistic.
#' \code{S} must have length greater than one.
#' @param type A character string. It specifies the type of test
#' the p-value function produces. The possible values are
#' \code{geq}, \code{leq}, \code{absolute} and \code{two-tailed}.
#'  Default is \code{geq}.
#'
#' @references Dufour, J.-M. (2006), Monte Carlo Tests with nuisance parameters:
#' A general approach to finite sample inference and nonstandard asymptotics in econometrics.
#' \emph{Journal of Econometrics}, \bold{133(2)}, 443-447.
#'
#' @references Dufour, J.-M. and Khalaf L. (2003), Monte Carlo Test Methods in Econometrics.
#' in Badi H. Baltagi, ed., \emph{A Companion to Theoretical Econometrics}, Blackwell Publishing Ltd, 494-519.
#'
#' @return The p-value of the statistic \code{S0} given a vector of replications \code{S}.
#' @export
#'
#' @example /inst/examples/pvalue_example.R
#'
pvalue <- function(S0, S, type = c("geq", "leq", "absolute", "two-tailed")) {

    if (any(is.na(S))){
        # Extract the number of simulation
        N <- length(S)
        # Drop NaN
        S <- stats::na.omit(S)
        if (length(S)!=0){
            warning(paste("simulated statistics contained NaN:", N-length(S), "replications were omitted"), immediate. = TRUE)
        }
        else{
            stop("simulated statistics vector is empty")
        }
    }

    # Extract the number of simulation
    N <- length(S)
    # Combine S0 and S to obtain the total set where we rank S0
    set.S <- c(S0, S)
    # Take the absolute value of S0 and S if p-value type is "absolute"
    if (type == "absolute") {
        set.S <- abs(set.S)
    }
    # Rank S0 and S
    set.u <- stats::runif(length(set.S[set.S == set.S[1]]))
    rank.S <- length(set.S[set.S < set.S[1]]) + length(set.u[set.u <= set.u[1]])
    # Compute the survival function
    survival.fun <- (N + 1 - rank.S[1])/N
    # Compute the p-value
    if (type == "absolute" || type == "geq") {
        pval <- (N * survival.fun + 1)/(N + 1)

    } else if (type == "leq") {
        pval <- (N * (1 - survival.fun) + 1)/(N + 1)

    } else if (type == "two-tailed") {
        pval <- 2 * min((N * (1 - survival.fun) + 1)/(N + 1), (N * survival.fun + 1)/(N + 1))
    }
    return(pval)
}
