
#' Sample from Bayesian copula GLM
#' 
#' Sample from a GLM via Bayesian copula regression model.
#' Uses random-walk Metropolis to update regression coefficients and dispersion parameters.
#' Assumes Inverse Wishart prior on augmented data.
#' 
#' @importFrom stats coef cor glm model.matrix cov sd
#' 
#' @param formula.list A \eqn{J}-dimensional list of formulas giving how the endpoints are related to the covariates
#' @param family.list A \eqn{J}-dimensional list of families giving how each endpoint is distributed. See \code{help(family)}
#' @param data A \code{data frame} containing all response variables and covariates. Variables must be named.
#' @param histdata \emph{Optional} historical data set for power prior on \eqn{\beta, \phi}
#' @param b0 \emph{Optional} power prior hyperparameter. Ignored if \code{is.null(histdata)}. Must be a number between \eqn{(0, 1]} if \code{histdata} is not \code{NULL}
#' @param c0 A \eqn{J}-dimensional vector for \eqn{\beta \mid \phi} prior covariance. If \code{NULL}, sets \eqn{c_0 = 10000} for each endpoint
#' @param alpha0 A \eqn{J}-dimensional vector giving the shape hyperparameter for each dispersion parameter on the prior on \eqn{\phi}. If \code{NULL} sets \eqn{\alpha_0 = .01} for each dispersion parameter
#' @param gamma0 A \eqn{J}-dimensional vector giving the rate hyperparameter for each dispersion parameter on the prior on \eqn{\phi}. If \code{NULL} sets \eqn{\alpha_0 = .01} for each dispersion parameter
#' @param Gamma0 Initial value for correlation matrix. If \code{NULL} defaults to the correlation matrix from the responses.
#' @param S0beta A \eqn{J}-dimensional \code{list} for the covariance matrix for random walk metropolis on beta. Each matrix must have the same dimension as the corresponding regression coefficient. If \code{NULL}, uses \code{solve(crossprod(X))}
#' @param sigma0logphi A \eqn{J}-dimensional \code{vector} giving the standard deviation on \eqn{\log(\phi)} for random walk metropolis. If \code{NULL} defaults to \code{0.1}
#' @param v0 An integer scalar giving degrees of freedom for Inverse Wishart prior. If \code{NULL} defaults to J + 2
#' @param V0 An integer giving inverse scale parameter for Inverse Wishart prior. If \code{NULL} defaults to \code{diag(.001, J)}
#' @param beta0 A \eqn{J}-dimensional \code{list} giving starting values for random walk Metropolis on the regression coefficients. If \code{NULL}, defaults to the GLM MLE
#' @param phi0  A \eqn{J}-dimensional \code{vector} giving initial values for dispersion parameters. If \code{NULL}. Dispersion parameters will always return 1 for binomial and Poisson models
#' @param M Number of desired posterior samples after burn-in and thinning
#' @param burnin burn-in parameter
#' @param thin post burn-in thinning parameter
#' @param adaptive logical indicating whether to use adaptive random walk MCMC to estimate parameters. This takes longer, but generally has a better acceptance rate
#' 
#' @return A named list. 
#' \code{["betasample"]} gives a \eqn{J}-dimensional list of sampled coefficients as matrices. 
#' \code{["phisample"]} gives a \eqn{M \times J} matrix of sampled dispersion parameters. 
#' \code{["Gammasample"]} gives a \eqn{J \times J \times M} array of sampled correlation matrices. 
#' \code{["betaaccept"]} gives a \eqn{M \times J} matrix where each row indicates whether the proposal for the regression coefficient was accepted.
#' \code{["phiaccept"]} gives a \eqn{M \times J} matrix where each row indicates whether the proposal for the dispersion parameter was accepted
#' @examples 
#' set.seed(1234)
#' n <- 100
#' M <- 100
#' 
#' x <- runif(n, 1, 2)
#' y1 <- 0.25 * x + rnorm(100)
#' y2 <- rpois(n, exp(0.25 * x))
#' 
#' formula.list <- list(y1 ~ 0 + x, y2 ~ 0 + x)
#' family.list <- list(gaussian(), poisson())
#' data = data.frame(y1, y2, x)
#' 
#' ## Perform copula regression sampling with default
#' ## (noninformative) priors
#' sample <- bayescopulaglm(
#'   formula.list, family.list, data, M = M, burnin = 0, adaptive = F
#' )
#' ## Regression coefficients
#' summary(do.call(cbind, sample$betasample))
#' 
#' ## Dispersion parameters
#' summary(sample$phisample)
#' 
#' ## Posterior mean correlation matrix
#' apply(sample$Gammasample, c(1,2), mean)
#' 
#' ## Fraction of accepted betas
#' colMeans(sample$betaaccept)
#' 
#' ## Fraction of accepted dispersion parameters
#' colMeans(sample$phiaccept)
#' @export
bayescopulaglm <- function(
  formula.list,
  family.list,
  data,
  histdata = NULL,
  b0 = NULL,
  c0 = NULL,
  alpha0 = NULL,
  gamma0 = NULL,
  Gamma0 = NULL,
  S0beta = NULL,
  sigma0logphi = NULL,
  v0 = NULL,
  V0 = NULL,
  beta0 = NULL,
  phi0 = NULL,
  M = 10000,
  burnin = 2000,
  thin = 1,
  adaptive = TRUE
) {
  
  ## First, obtain burn-in sample
  if ( burnin > 0 ) {
    
    ## burn-in sample
    smpl <- bayescopulaglm_wrapper(
      formula.list, family.list, data, M = burnin, histdata, b0,
      c0, alpha0, gamma0, Gamma0, S0beta, sigma0logphi, v0, V0,
      beta0, phi0, thin = 1
    )
    
    ## Obtain starting values for new chain as last values for burn chain
    beta0 <- lapply(smpl$betasample, function(x) as.matrix(x)[nrow(x), ] )
    beta0 <- lapply(beta0, as.numeric)
    phi0 <- smpl$phisample[burnin, ]
    Gamma0 <- smpl$Gammasample[, , burnin]
    
    ## Update proposals if adaptive == TRUE
    if ( adaptive ) {
      ## Updated beta covariance for proposal
      cd.sq <- sapply(smpl$betasample, ncol)
      cd.sq <- 2.4 ^ 2 / cd.sq
      smpl$betasample <- lapply(smpl$betasample, function(x) as.matrix(x)[-(1:ceiling(burnin / 2)), , drop = F] )
      S0beta <- mapply(function(a, b) a * cov(b), a = cd.sq, b = smpl$betasample, SIMPLIFY = FALSE)
      
      ## Updated logphi variance for proposal
      sigma0logphi <- apply( log( smpl$phisample[-(1:ceiling(burnin/2)), ] ), 2, sd )             ## returns 0 for discrete params
      sigma0logphi <- ifelse(sigma0logphi == 0, .1, sigma0logphi)       ## change 0 to positive to avoid errors--does not affect sampling
    }
  }
  
  ## Now, obtain post burn-in sampling accounting for thinning
  smpl <- bayescopulaglm_wrapper(
    formula.list, family.list, data, M = M, histdata, b0,
    c0, alpha0, gamma0, Gamma0, S0beta, sigma0logphi, v0, V0,
    beta0, phi0, thin = thin
  )
  
  ## Store formula.list and family.list for easy passing onto other functions e.g. predict
  smpl$formula.list <- formula.list
  smpl$family.list <- family.list
  class(smpl) <- c(class(smpl), 'bayescopulaglm')
  
  return(smpl)
}








#' Sample from Bayesian copula GLM helper function
#' 
#' Helper function to sample from any GLM via Bayesian copula regression as developed in Pitt et al. (2006).
#' Uses random-walk Metropolis to update regression coefficients and dispersion parameters.
#' Assumes Inverse Wishart prior on augmented data with covariance matrix as developed by Hoff (2007).
#' 
#' 
#'
#' @param formula.list A \eqn{J}-dimensional list of formulas giving how the endpoints are related to the covariates
#' @param family.list A \eqn{J}-dimensional list of families giving how each endpoint is distributed. See \code{help(family)}
#' @param data A \code{data frame} containing all response variables and covariates. Variables must be named.
#' @param M Number of desired posterior samples
#' @param histdata \emph{Optional} historical data set for power prior on \eqn{\beta, \phi}
#' @param b0 \emph{Optional} power prior hyperparameter
#' @param c0 A \eqn{J}-dimensional vector for \eqn{\beta \mid \phi} prior covariance: \eqn{\text{Cov}(\beta \mid \phi) = c_0 \phi I_n }. If \code{NULL}, sets \eqn{c_0 = 10000} for each endpoint
#' @param alpha0 A \eqn{J}-dimensional vector giving the shape hyperparameter for each dispersion parameter on the prior on \eqn{\phi}. If \code{NULL} sets \eqn{\alpha_0 = .01} for each dispersion parameter
#' @param gamma0 A \eqn{J}-dimensional vector giving the rate hyperparameter for each dispersion parameter on the prior on \eqn{\phi}. If \code{NULL} sets \eqn{\alpha_0 = .01} for each dispersion parameter
#' @param Gamma0 Initial value for correlation matrix. If \code{NULL} defaults to the correlation matrix from the responses.
#' @param S0beta A \eqn{J}-dimensional \code{list} for the covariance matrix for random walk metropolis on beta. Each matrix must have the same dimension as the corresponding regression coefficient. If \code{NULL}, uses \code{solve(crossprod(X))}
#' @param sigma0logphi A \eqn{J}-dimensional \code{vector} giving the standard deviation on \eqn{\log(\phi)} for random walk metropolis. If \code{NULL} defaults to \code{0.1}
#' @param v0 An integer scalar giving degrees of freedom for Inverse Wishart prior. If \code{NULL} defaults to J + 2
#' @param V0 An integer giving inverse scale parameter for Inverse Wishart prior. If \code{NULL} defaults to \code{diag(.001, J)}
#' @param beta0 A \eqn{J}-dimensional \code{list} giving starting values for random walk Metropolis on the regression coefficients. If \code{NULL}, defaults to the GLM MLE
#' @param phi0  A \eqn{J}-dimensional \code{vector} giving initial values for dispersion parameters. If \code{NULL}. Dispersion parameters will alwyas return 1 for binomial and Poisson models
#' @param thin thinning parameter
#' 
#' @return A named list. 
#' \code{["betasample"]} gives a \eqn{J}-dimensional list of sampled coefficients as matrices. 
#' \code{["phisample"]} gives a \eqn{M \times J} matrix of sampled dispersion parameters. 
#' \code{["Gammasample"]} gives a \eqn{J \times J \times M} array of sampled correlation matrices. 
#' \code{["betaaccept"]} gives a \eqn{M \times J} matrix where each row indicates whether the proposal for the regression coefficient was accepted.
#' \code{["phiaccept"]} gives a \eqn{M \times J} matrix where each row indicates whether the proposal for the dispersion parameter was accepted
#' @noRd
#' @keywords internal
bayescopulaglm_wrapper <- function(
  formula.list,
  family.list,
  data,
  M = 10000,
  histdata = NULL,
  b0 = NULL,
  c0 = NULL,
  alpha0 = NULL,
  gamma0 = NULL,
  Gamma0 = NULL,
  S0beta = NULL,
  sigma0logphi = NULL,
  v0 = NULL,
  V0 = NULL,
  beta0 = NULL,
  phi0 = NULL,
  thin = 1
) {
  ##
  ## define functions to get dependent variable and design matrix from formulas;
  ## then, save all dependent variables to matirx and design matrices as list.
  ## If histdata != NULL, do the same for historical data.
  ## 
  
  if ( class(formula.list) != 'list' ) {
    stop('formula.list must be a list of formulas')
  }
  if ( any( sapply(formula.list, class) != 'formula') ) {
    stop('At least one element of formula.list is not a formula')
  }
  
  J <- length(formula.list)
  
  if ( M <= 0 ) { stop("M must be a positive integer") }
  
  if ( class(data) != 'data.frame' ) {
    stop('data must be a data.frame')
  }
  if (!is.null(histdata)) {
    if(class(histdata) != 'data.frame') {
      stop('histdata must be a NULL or a data.frame')
    }
  }
  if ( is.null(b0) ) {
    if ( ! is.null(histdata) ) {
      stop('b0 must be a number in (0, 1] if histdata is specified')
    }
  }
  
  if ( !is.null(b0) ) {
    if( is.null(histdata) ) {
      stop('b0 must be NULL if histdata is NULL')
    }
  }
  famlist <- sapply(family.list, function(f)f$family)
  if ( any( !( famlist %in% c('gaussian', 'poisson', 'Gamma', 'binomial') ) ) ) {
    stop("Family must be one of gaussian, poisson, Gamma, or binomial")
  }
  
  
  if( is.null(b0) ) {
    b0 <- 0   ## corresponds to no historical data influence in C++ code
  }
  
  get_depvar <- function(f, data) {
    data[, all.vars(f, data)[1] ]
  }
  get_desmat <- function(f, data) {
    model.matrix(f, data)
  }
  
  ymat <- sapply(formula.list, get_depvar, data)
  Xlist <- lapply(formula.list, get_desmat, data)
  
  if( !is.null( histdata ) ) {
    y0mat <- sapply(formula.list, get_depvar, histdata)
    Xlist <- lapply(formula.list, get_desmat, histdata)
  } else {                                            ## need to give c++ code default values
    b0 <- 0
    y0mat <- matrix(rep(0, J), ncol = J)
    X0list <- lapply(rep(0, J), function(x) as.matrix(x))
    n0 <- 0
  }
  
  ## 
  ## Store name of distribution and name of link function
  ## as string vectors.
  ## 
  distnamevec <- sapply(family.list, function(x) x$family)
  linknamevec <- sapply(family.list, function(x) x$link)
  
  
  ## 
  ## Define hyperparameters if null
  ## 
  if ( is.null(alpha0) ) {
    alpha0 = rep(.1, J)
  }
  if ( is.null(gamma0) ) {
    gamma0 = rep(.1, J)
  }
  if ( length(alpha0) != J ) {
    stop('alpha0 must have length = number of endpoints')
  }
  
  if ( length(gamma0) != J ) {
    stop('gamma0 must have length = number of endpoints')
  }
  
  if ( is.null( S0beta ) ) {
    get_cov_mtx <- function(f, data) {
      X <- model.matrix(f, data)
      chol2inv(chol(crossprod(X)))
    }
    S0beta <- lapply(formula.list, get_cov_mtx, data = data)
  }
  
  if ( class(S0beta) != 'list' ) {
    stop('S0beta must be a list of matrices')
  }
  
  if ( is.null(sigma0logphi) ) {
    sigma0logphi <- rep(0.1, times = J)
  }
  if ( length(sigma0logphi) != J ) {
    stop('sigma0logphi must have length = number of endpoints')
  }
  if ( is.null(v0) ) {
    v0 <- J + 2
  }
  if ( v0 <= J ) {
    stop("v0 must be larger than number of endpoints")
  }
  if ( is.null(V0) ) {
    V0 <- diag(.001, J)
  }
  if (nrow(V0) != J | ncol(V0) != J) {
    stop('V0 must be a square matrix of dimension = number of endpoints')
  }
  if ( is.null(c0) ) {
    c0 <- rep(10000, J)
  }
  if ( length(c0) < J) {
    stop("c0 must have same length as formula.list")
  }
  if ( is.null(beta0) ) {
    get_glm_coef <- function(fmla, fam, data) {
      coef( glm( fmla, fam, data ) )
    }
    beta0 <- mapply(get_glm_coef, formula.list, family.list, MoreArgs = list('data' = data), SIMPLIFY = FALSE )
  }
  
  if ( class(beta0) != 'list' | length(beta0) != J | any(sapply(beta0, class) != 'numeric')) {
    stop('beta0 must be a list of dimension = number of endpoint and each element of the list must be of type numeric')
  }
  
  if ( is.null( phi0 ) ) {
    get_glm_disp <- function(fmla, fam, data) {
      summary( glm( fmla, fam, data ) )$dispersion
    }
    phi0 <- mapply(get_glm_disp, formula.list, family.list, MoreArgs = list('data' = data), SIMPLIFY = TRUE )
  }
  
  if ( length(phi0) != J ) {
    stop('phi0 must be a numeric vector with length = number of endpoints')
  }
  if ( is.null( Gamma0 ) ) {
    Gamma0 <- cor(ymat)
  }
  
  if (nrow(Gamma0) != J | ncol(Gamma0) != J) {
    stop('Gamma0 must be a square matrix of dimension = number of endpoints')
  }
  
  ## Call sample_copula_cpp function
  smpl <- sample_copula_cpp ( 
    ymat, Xlist, distnamevec, linknamevec, c0, S0beta, sigma0logphi,
    alpha0, gamma0, Gamma0, v0, V0, b0, y0mat, X0list, M, beta0, phi0, thin
  )
  
  ## Replace column names in smpl$betasample with corresponding name of regression coefficient
  get_col_names <- function(f, data) {
    colnames(model.matrix(f, data))
  }
  colNames <- lapply(formula.list, get_col_names, data = data)
  for(j in 1:J) {
    colnames( smpl$betasample[[j]] ) <- colNames[[j]]
  }
  smpl
}


