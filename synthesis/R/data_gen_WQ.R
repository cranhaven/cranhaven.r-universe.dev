#' Generate build-up and wash-off model for water quality modeling
#'
#' @param nobs The data length to be generated.
#' @param k build-up coefficient (kg*t-1)
#' @param a wash-off rate constant (m-3)
#' @param m0 threshold at which additional mass does not accumulate on the surface (kg)
#' @param q runoff (m3*t-1)
#'
#' @return A list of 2 elements: a vector of build-up mass (x), and a vector of wash-off mass (y) per unit time.
#' @export
#' @importFrom stats rnorm
#'
#' @references
#' Wu, X., Marshall, L., & Sharma, A. (2019). The influence of data transformations in simulating Total Suspended Solids using Bayesian inference. Environmental modelling & software, 121, 104493. doi:https://doi.org/10.1016/j.envsoft.2019.104493
#'
#' Shaw, S. B., Stedinger, J. R., & Walter, M. T. (2010). Evaluating Urban Pollutant Buildup/Wash-Off Models Using a Madison, Wisconsin Catchment. Journal of Environmental Engineering, 136(2), 194-203. https://doi.org/10.1061/(ASCE)EE.1943-7870.0000142
#'
#' @examples
#' # Build up model
#' set.seed(101)
#' sample = 500
#' #create a gamma shape storm event
#' q<- seq(0,20, length.out=sample)
#' p <- pgamma(q, shape=9, rate =2, lower.tail = TRUE)
#' p <- c(p[1],p[2:sample]-p[1:(sample-1)])
#'
#' data.tss<-data.gen.BUWO(sample, k=0.5, a=5, m0=10, q=p)
#' plot.ts(cbind(p, data.tss$x, data.tss$y), ylab=c("Q","Bulid-up","Wash-off"))

data.gen.BUWO <- function(nobs, k=0.5, a=1, m0=10, q=0) {
    nwarm <- 500
    n <- nobs + nwarm
    x <- matrix(NA, n, 1)
    for (i in 1:nwarm) {
        x[i] <- rnorm(1, mean = 0, sd = 1)
    }

    if(length(q)==1) q=rep(q,nobs)
    y <- matrix(NA, nobs, 1)
    for (i in (nwarm + 1):n) {

        x[i] <- x[i-1] + k*(1-x[i-1]/m0) - a*x[i-1]*q[i-nwarm]
        y[i-nwarm] <- a*x[i-1]*q[i-nwarm]
    }

    x <- x[(n - nobs + 1):n]
    data_generated <- list(x = x, y = y)
    return(data_generated)
}


