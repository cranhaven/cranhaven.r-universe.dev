#' Critical values for the Equal Allocation 3, Proportional Allocation 2,
#' and Equal Allocation 2 procedures
#'
#' Computes  the critical values
#' for null hypotheses rejection and corresponding nominal two-sided significance
#' levels for the Equal Allocation 3, Proportional Allocation 2,
#' and Equal Allocation 2 procedures
#'
#'
#' @param corAa  correlation between the overall A and simple A log hazard ratio estimates
#' @param corAab  correlation between the overall A and simple AB log hazard ratio estimates
#' @param coraab  correlation between the simple A and simple AB log hazard ratio estimates
#' @param niter  number of times we compute the critical values to average out
#' the randomness from the \code{pmvnorm} function call
#' @param alpha  two-sided familywise error level to control
#' @param dig number of decimal places to which we \code{\link{roundDown}} the critical value
#' @param abseps \code{abseps} setting in the \code{pmvnorm} function call
#' @param tol  \code{tol} setting in the uniroot function call
#'
#' @return
#' \item{critEA3 }{Equal Allocation 3 procedure's critical value for all three test statistics}
#' \item{sigEA3 }{two-sided nominal significance level corresponding to \code{critEA3}}
#' \item{critPA2A }{Proportional Allocation 2 procedure's critical value for the overall A statistic}
#' \item{sigPA2A }{two-sided nominal significance level corresponding to \code{critPA2A}}
#' \item{critPA2ab }{Proportional Allocation 2 procedure's critical value for the simple AB statistic}
#' \item{sigPA2ab }{two-sided nominal significance level corresponding to \code{critPA2ab}}
#' \item{critEA2 }{Equal Allocation 2 procedure's critical value for the simple A and AB statistics}
#' \item{sigEA2 }{two-sided nominal significance level corresponding to \code{critEA2}}
#'
#' @details This function computes the Dunnett-corrected critical values
#' based on the asymptotic correlations of the overall A, simple A, and simple AB
#' logrank statistics as described in Leifer, Troendle, et al. (2020) and are derived in
#' Lin, Gong, et al. (2016) and Slud (1994).  \code{pmvnorm} uses a random seed in its algorithm.
#' To smooth out the randomness,  \code{pmvnorm} is called \code{niter} times.
#' The \code{roundDown} function is used in conjunction with the \code{dig} argument
#' to insure that any rounding of the (negative) critical values will be done conservatively to control
#' the familywise type I error at the desired level.
#' @references Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D.
#' Joint testing of overall and simple effect for the two-by-two factorial design. 2020. Submitted.
#' @references Lin, D-Y., Gong, J., Gallo, P., et al. Simultaneous inference on treatment effects
#' in survival studies with factorial designs. Biometrics. 2016; 72: 1078-1085.
#' @references Slud, E.V. Analysis of factorial survival experiments. Biometrics. 1994; 50: 25-38.
#' @export crit2x2
#' @seealso \code{roundDown}. \code{eventProb}, \code{lgrkPower}, \code{strLgrkPower}, \code{pmvnorm}
#' @export crit2x2
#' @examples
#' # Example 1:  Compute the nominal significance levels for rejection using
#' # the asymptotic correlations derived in Slud (1994)
#' corAa  <- 1/sqrt(2)
#' corAab <- 1/sqrt(2)
#' coraab <- 1/2
#'
#' crit2x2(corAa, corAab, coraab, dig = 2, alpha = 0.05, niter = 5)
#' # critEA3
#' # [1] -2.32
#'
#' # sigEA3
#' # [1] 0.02034088
#'
#' # critPA2A
#' # [1] -2.13
#'
#' # sigPA2A
#' # [1] 0.03317161
#'
#' # critPA2ab
#' # [1] -2.24
#'
#' # sigPA2ab
#' # [1] 0.02509092
#'
#' # critEA2
#' # [1] -2.22
#'
#' # sigEA2
#' # [1] 0.02641877
#'
#' # Example 2:  Compute the nominal critical values and significance levels for rejection
#' # using the estimated correlations for simdat.
#' corAa  <- 0.6123399
#' corAab <- 0.5675396
#' coraab <- 0.4642737
#'
#' crit2x2(corAa, corAab, coraab, dig = 2, alpha = 0.05, niter = 5)
#' # $critEA3
#' # [1] -2.34
#'
#' # $critPA2A
#' # [1] -2.13
#'
#' # $sigPA2A
#' # [1] 0.03317161
#'
#' # $critPA2ab
#' # [1] -2.3
#'
#' # $sigPA2ab
#' # [1] 0.02144822
#' #
#' # $sigEA3
#' # [1] 0.01928374
#'
#' # $critEA2
#' # [1] -2.22
#'
#' # $sigEA2
#' # [1] 0.02641877

crit2x2 <- function(corAa, corAab, coraab, dig = 2,
	alpha = 0.05,  niter = 5,  abseps = 1e-05, tol = 1e-04){
# This functions requires the "mvtnorm" package and
  # the output from the cor2x2 function.
  # NOTE:  increasing niter will slow down the function
# This function computes the critical values for the
# Proportional Allocation 2, Equal Allocation 3, and Equal Allocation 2 procedures, respectively.
# We have parametrized the problem so that large negative Z values
# are rejected.
# The inputs are:
# corAa = correlation between the overall A and simple A log hazard
# 		ratio (LHR) estimates
# corAab = correlation between the overall A and simple AB LHR estimates
# coraab = correalation between the simple A and simple AB estimates
# niter = number of times we compute the critical values to average the
#	    the out the randomness from the mvtnorm calculation
# alpha = two-sided significance level to control
# rseed = random seed used for the multivariate normal integration
# abseps = abseps setting in the pmvnorm function calls
# tol = tol setting in the uniroot function calls
# Notes :  1) corAa, corAab, and coraab may be computed using the
#		LinCov function based on the Lin et al. (2016, Biometrics)
#		methodology

# Previous versions of crit2x2 set a random seed here
# to be used in conjunction with the pmvnorm call.  CRAN
# suggested that this be omitted.
# set.seed(rseed)

# Initialize the results vectors.
	sigPA2A <- rep(0, niter)
	critPA2A <- rep(0, niter)
	sigPA2ab <- rep(0, niter)
	critPA2ab <- rep(0, niter)
	sigEA3 <- rep(0, niter)
	critEA3 <- rep(0, niter)
	sigEA2 <- rep(0, niter)
	critEA2 <- rep(0, niter)

	for(i in 1:niter){
# Compute the critical values for the Proportional Allocation 2 procedure.
# Obtain the two-sided nominal significance level for the overall A test.
	sigPA2A[i] <- 2*alpha/3

# Obtain the critical for the overall A test
	critPA2A[i] <- qnorm(alpha/3)

# Obtain the two-sided nominal significance level for the simple AB test.
	cormat23 <- matrix(c(1, corAab, corAab, 1), byrow = TRUE, nrow = 2)
	auxfcn23 <- function(z){
  				(1 - pmvnorm(lower=-Inf, upper=
					-c(qnorm(alpha/3),
				qnorm(z)), mean=c(0,0),
               		corr=cormat23, sigma=NULL, maxpts = 25000,
				abseps = abseps, releps = 0)) - alpha/2
	}
	sigPA2ab[i] <- 2 * uniroot(auxfcn23, lower = alpha/6,
						upper = alpha/2, tol = tol)$root

# Obtain the critical value for the simple AB test
	critPA2ab[i] <- qnorm(sigPA2ab[i]/2)

# Obtain the two-sided nominal significance level for all 3 tests for the
#	Equal Allocation 3 procedure.

	cormat13 <- matrix(c(1, corAa, corAab,
                    	corAa, 1, coraab,
                    	corAab, coraab, 1), byrow=TRUE, nrow = 3)
	auxfcn13 <- function(z, tol = tol){
 		(1 - pmvnorm(lower = c(qnorm(z), qnorm(z), qnorm(z)),
 		   upper= -c(qnorm(z), qnorm(z), qnorm(z)), mean=c(0,0,0),
        	corr=cormat13, sigma=NULL, maxpts = 25000, abseps = abseps,
       	 releps = 0)) - alpha
	}
# 	set.seed(rseed)
	sigEA3[i] <- 2 * uniroot(auxfcn13, lower = alpha/6,
						upper = alpha/2, tol = tol)$root
# Obtain the critical value for each of the three tests in the Equal Allocation 3
# procedure.
	critEA3[i] <- qnorm(sigEA3[i]/2)

# Obtain the two-sided nominal significance level for each test of the
#	Equal Allocation 2 procedure.
	cormat12 <- matrix(c(1, coraab, coraab, 1), byrow = TRUE, nrow = 2)
	auxfcn12 <- function(z, tol = tol){
  				(1 - pmvnorm(lower=-Inf, upper=-c(qnorm(z),
					qnorm(z)), mean=c(0,0),
               			corr=cormat12, sigma=NULL, maxpts = 25000,
					abseps = abseps, releps = 0)) - alpha/2  ### I FIXED THIS LINE, subtract alpha/2, not 0.025
			}
#	set.seed(rseed)
	sigEA2[i] <- 2 * uniroot(auxfcn12, lower = alpha/6,
						upper = alpha, tol = tol)$root
# Obtain the critical value for each test in the Equal Allocation 2
# procedure.
	critEA2[i] <- qnorm(sigEA2[i]/2)
	}
	resultsmat <- cbind(sigPA2A, critPA2A, sigPA2ab, critPA2ab, sigEA3,
					critEA3, sigEA2, critEA2)
	dimnames(resultsmat)[[2]] <- NULL
	aux <- apply(resultsmat, 2, mean)
# apply roundDown to the critical values for CONSERVATISM
	for(i in c(2, 4, 6, 8)){
	  aux[i] <- roundDown(aux[i], dig)
	}
# compute the corresponding nominal significance levels, AGAIN cONSERVATIVE
	for(i in c(1, 3, 5, 7)){
	  aux[i] <- 2 * pnorm(aux[i + 1])
	}
	list(critEA3 = aux[6], sigEA3 = aux[5],
		critPA2A = aux[2], sigPA2A = aux[1],
		critPA2ab = aux[4], sigPA2ab = aux[3],
		critEA2 = aux[8], sigEA2 = aux[7])
}






