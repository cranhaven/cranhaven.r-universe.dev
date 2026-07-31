#' Generate Overdispersed Binomial Outcome Data
#'
#' Using a three step algorithm to generate overdispersed binomial outcome data.
#' When the number of frequencies, binomial random variable, probability of success and overdispersion
#' are given.
#'
#' @usage
#' GenerateBOD(N,n,pi,rho)
#'
#' @param N               single value for number of total frequencies
#' @param n               single value for binomial random variable
#' @param pi              single value for probability of success
#' @param rho             single value for overdispersion parameter
#'
#' @details
#' The generated binomial random variables are overdispersed based on \eqn{rho} for the probability of
#' success \eqn{pi}.
#'
#' Step 1: Solve the following equation for a given \eqn{n,pi,rho},
#' \deqn{phi(z(pi),z(pi),delta)=pi(1-pi)rho + pi^2,}
#'
#' For \eqn{delta} where \eqn{phi(z(pi),z(pi),delta)} is the cumulative distribution function of the
#' standard bivariate normal random variable with correlation coefficient \eqn{delta}, and \eqn{z(pi)} denotes
#' the \eqn{pi^{th}} quantile of the standard normal distribution.
#'
#' Step 2: Generate $n$-dimensional multivariate normal random variables, \eqn{Z_i=(Z_{i1},Z_{i2},ldots,Z_{in})^T}
#' with mean \eqn{0} and constant correlation matrix \eqn{Sigma_i} for \eqn{i=1,2,\ldots,N,} where the elements of
#' \eqn{(Sigma_i)_{lm}} are \eqn{delta} for \eqn{l \ne m}.
#'
#' Step 3: Now for each \eqn{j=1,2,\ldots,n} define \eqn{X_{ij} = 1;} if \eqn{Z_{ij} < z(\pi)}, or
#' \eqn{X_{ij} = 0;} otherwise. Then, it can be showed that the random variable \eqn{Y_i=\sum_{j=1}^{n} X_{ij}}
#' is overdispersed relative to the Binomial distribution.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{GenerateBOD} gives a vector of overdispersed binomial random variables
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#'
#' @examples
#' N <- 500    # Number of observations
#' n <- 10      # Dimension of multivariate normal random variables
#' pi <- 0.5   # Probability threshold
#' rho <- 0.1  # Dispersion parameter
#'
#' # Generate overdispersed binomial variables
#' New_overdispersed_data <- GenerateBOD(N, n, pi, rho)
#' table(New_overdispersed_data)
#'
#' @importFrom Rdpack reprompt
#' @export
GenerateBOD<-function(N, n, pi, rho)
{
  if(any(is.na(c(N,n,pi,rho))) | any(is.nan(c(N,n,pi,rho)))){
    stop("NA or Infinite or NAN values in the N,n,pi or rho")
  }

  if((length(N)+length(n)+length(pi)+length(rho))!= 4){
    stop("N, n, pi or rho has a value greater than length one")
  }

  if(N <= 0 | n <= 0 ){
    stop("N or n cannot be negative values")
  }

  if(pi <= 0 | pi >= 1){
    stop("probability of success cannot be greater than or equal to one \nor less than or equal to zero")
  }
  if(rho < 0 ){
    stop("dispersion parameter rho cannot be less than zero")
  }

  # Function to solve for delta, copied for completeness
  z_pi <- stats::qnorm(pi)

  # Step 1: Solve for delta
  delta <- stats::uniroot(function(delta) {
    lhs <- mvtnorm::pmvnorm(lower = rep(-Inf, 2), upper = rep(z_pi, 2), mean = c(0, 0),
                            sigma = matrix(c(1, delta, delta, 1), nrow = 2))
    rhs <- pi * (1 - pi) * rho + pi^2
    # Return the difference between lhs and rhs directly
    return(lhs - rhs)
  }, lower = -1, upper = 1, tol = .Machine$double.eps^0.25)$root
  # Assuming solve_for_delta is defined from the previous step

  # Step 2: Generate multivariate normal random variables
  Sigma <- matrix(delta, n, n)  # Set all elements to delta
  diag(Sigma) <- 1              # Reset diagonal elements to 1

  Z <- MASS::mvrnorm(n = N, mu = rep(0, n), Sigma = Sigma)

  # Step 3: Transform Z_ij into X_ij and calculate Y_i
  z_pi <- stats::qnorm(pi)  # Calculate the pi-th quantile of the standard normal distribution
  X <- ifelse(Z < z_pi, 1, 0)
  Y <- rowSums(X)
  return(Y)
}

#' @importFrom MASS mvrnorm
#' @importFrom stats uniroot
#' @importFrom stats qnorm
#' @importFrom mvtnorm pmvrnorm
