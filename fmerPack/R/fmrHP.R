##' Finite Mixture Effects Model with Heterogeneity Pursuit
##'
##' Produce solution paths of regularized finite mixture effects model with 
##' lasso or adaptive lasso penalty; compute the degrees of freeom, likelihood 
##' and information criteria (AIC, BIC and GIC) of the estimators. 
##' Model fitting is conducted by EM algorithm and Bregman coordinate descent.
##'
##' Model parameters can be specified through argument \code{modstr}. The
##' available include
##' \itemize{
##'    \item{lambda}: A vector of user-specified lambda values with default NULL.
##'    
##'    \item{lambda.min.ratio}: Smallest value for lambda, as a fraction of lambda.max, 
##'        the (data derived) entry value.
##'    
##'    \item{nlambda}: The number of lambda values.
##'    
##'    \item{w}: Weight matrix for penalty function. Default option is NULL, which means 
##'        lasso penailty is used for model fitting.
##'    
##'    \item{intercept}: Should intercept(s) be fitted (default=TRUE) or set to zero (FALSE).
##'    
##'    \item{common.only}: A vector of user-specified indicators of the variables 
##'        only with common effects.
##'      
##'    \item{common.no.penalty}: A vector of user-specified indicators of the variables
##'        with no penalty on the common effect.
##'    
##'    \item{cluster.no.penalty}: A vector of user-specified indicators of the variables
##'        with no penalty on the cluster-specific effects.
##'        
##'    \item{select.ratio}: A user-specified ratio indicating the ratio of variables to be selected.
##' }
##'
##' The available elements for argument \code{control} include
##' \itemize{
##'    \item{epsilon}: Convergence threshold for generalized EM algorithm.
##'        Defaults value is 1E-6.
##'    
##'    \item{maxit}: Maximum number of passes over the data for all lambda values.
##'        Default is 1000.
##'    
##'    \item{inner.eps}: Convergence threshold for Bregman coordinate descent algorithm.
##'        Defaults value is 1E-6.
##'    
##'    \item{inner.maxit}: Maximum number of iteration for Bregman coordinate descent algorithm.
##'        Defaults value is 200.
##'    
##'    \item{n.ini}: Number of initial values for EM algorithm. Default is 10. In EM algorithm, it is 
##'        preferable to start from several different initial values.
##' }
##'
##' @usage
##' path.fmrHP(y, X, m, equal.var = FALSE, 
##'            ic.type = "ALL", B = NULL, prob = NULL, rho = NULL, 
##'            control = list(), modstr = list(), report = FALSE)
##'
##' @param y a vector of response (\eqn{n \times 1})
##' @param X a matrix of covariate (\eqn{n \times p})
##' @param m number of components
##' @param equal.var indicating whether variances of different components are equal
##' @param ic.type the information criterion to be used; currently supporting "AIC", "BIC", and "GIC".
##' @param B initial values for the rescaled coefficients with first column being the 
##'     common effect, and the rest \code{m} columns being the heterogeneity for 
##'     corresponding components
##' @param prob initial values for prior probabilitis for different components
##' @param rho initial values for rho vector (\eqn{1 / \sigma}), the reciprocal of standard deviation
##' @param control a list of parameters for controlling the fitting process
##' @param modstr a list of model parameters controlling the model fitting
##' @param report indicating whether printing the value of objective function during EM algorithm 
##'     for validation checking of initial value.
##'
##' @return
##' A list consisting of
##' \item{lambda}{vector of lambda used in model fitting}
##' \item{lambda.used}{vector of lambda in model fitting after truncation by select.ratio}
##' \item{B.hat}{estimated rescaled coefficient (\eqn{p \times m + 1 \times nlambda})}
##' \item{pi.hat}{estimated prior probabilities (\eqn{m \times nlambda})}
##' \item{rho.hat}{estimated rho values (\eqn{m \times nlambda})}
##' \item{IC}{values of information criteria}
##'
##' @examples
##' \donttest{
##' library(fmerPack)
##' ## problem settings
##' n <- 100; m <- 3; p <- 5;
##' sigma2 <- c(0.1, 0.1, 0.4); rho <- 1 / sqrt(sigma2)
##' phi <- rbind(c(1, 1, 1), c(1, 1, 1), c(1, 1, 1), c(-3, 3, 0), c(3, 0, -3))
##' beta <- t(t(phi) / rho)
##' ## generate response and covariates
##' z <- rmultinom(n, 1, prob= rep(1 / m, m))
##' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
##' y <- MASS::mvrnorm(1, mu = rowSums(t(z) * X[, 1:(nrow(beta))] %*% beta), 
##'                    Sigma = diag(colSums(z * sigma2)))
##' ## lasso
##' fit1 <- path.fmrHP(y, X, m = m, modstr = list(nlambda = 10), control = list(n.ini = 1))
##' ## adaptive lasso
##' fit2 <- path.fmrHP(y, X, m = m, 
##'                    modstr = list(w = abs(select.tuning(fit1)$B + 1e-6)^2))
##' }
##' @references 
##' Li, Y., Yu, C., Zhao, Y., Yao, W., Aseltine, R. H., & Chen, K. (2021). 
##' Pursuing Sources of Heterogeneity in Modeling Clustered Population.
##' @import flexmix glmnet
##' @importFrom abind abind
##' @export

path.fmrHP <- function(y, X, m, equal.var = FALSE,
                       ic.type = "ALL",
                       B = NULL,
                       prob = NULL,
                       rho = NULL,
                       control = list(),
                       modstr = list(),
                       report = FALSE) {
  Call <- match.call()
  ## set control values for model fitting
  control <- do.call("fmrHP.control", control)
  n <- length(y)
  p <- ncol(X)
  ## set mod strings
  modstr <- do.call("fmrHP.modstr", modstr)
  lambda <- modstr$lambda
  lambda.min.ratio <- modstr$lambda.min.ratio
  nlambda <- modstr$nlambda
  select.ratio <- modstr$select.ratio
  ## intercept?
  intercept <- modstr$intercept
  w <- modstr$w
  if(is.null(w)) {
    w <- matrix(1, p, m + 1)
    if(intercept) {
      w <- rbind(c(0, rep(1, m)), w)
    }
  } else {
    ## check any Inf to avoid error
    w[is.infinite(w)] <- 1e6
    if(any(w < 0)) {
      stop("weight cannot be negative")
    }
    if(nrow(w) != p + as.numeric(intercept)) {
      stop("Dimensions of weight matrix don't match number of covariates")
    }
  }
  ## check the user-specific penalty options: common.only, common.no.penalty and cluster.no.penalty
  #### variables without penalty on common effect
  common.only <- modstr$common.only
  cluster.no.penalty <- modstr$cluster.no.penalty
  common.no.penalty <- modstr$common.no.penalty
  if(! is.null(common.no.penalty)) {
    if(! (is.vector(common.no.penalty) && length(common.no.penalty) <= p)) {
      stop(paste0("The common.no.penalty parameter needs to be a vector with dimension not greater than ", p))
    } else {
      w[common.no.penalty + as.numeric(intercept), 1] <- 0
    }
  }
  #### check whether intersect between the variables without cluster effects and 
  #### variables without penalty on cluster effects
  if(any(intersect(cluster.no.penalty, common.only))) {
    stop("The 'cluster.no.penalty' and 'common.only' cannot have intersection")
  }
  #### variables without cluster-specific effect
  if(! is.null(cluster.no.penalty)) {
    if(! (is.vector(cluster.no.penalty) && length(cluster.no.penalty) <= p)) {
      stop(paste0("The cluster.no.penalty parameter needs to be a vector with dimension not greater than ", p))
    } else {
      w[cluster.no.penalty + as.numeric(intercept), -1] <- 0
    }
  }
  #### variables with only common effect
  if(! is.null(common.only)) {
    if(! (is.vector(common.only) && length(common.only) <= p)) {
      stop(paste0("The common.only parameter needs to be a vector with dimension not greater than ", p))
    } else {
      w[common.only + as.numeric(intercept), -1] <- Inf
    }
  }
  ## set initial values, if some of the values exist, then keep them
  ini.mix <- ini.EM(y = y, X = X, B = B, prob = prob, rho = rho, n = n, p = p, m = m,
                    inner.maxit = control$inner.maxit, n.ini = control$n.ini,
                    report = report, intercept = intercept)
  ini.B <- ini.mix$B
  ini.pi <- ini.mix$prob
  ini.rho <- ini.mix$rho
  ## list of lambda
  if (is.null(lambda)) {
    lambda.max <- lambdamax(y, X)
    lambda.min <- lambda.min.ratio * lambda.max
    lambda <- exp(seq(log(lambda.max), log(lambda.min), length = nlambda))
  } else {
    nlambda <- length(lambda)
  }
  lambda <- rev(sort(lambda))
  lambda.used <- NULL  ## record the truly used sequence of lambda
  ## fit for sequence of lambda
  ## output
  out.B <- out.pi <- out.rho <- out.info <- NULL
  for(lam in lambda) {
      tmp <- fmrHP(y, X, m, intercept = intercept,
                   lambda = lam, equal.var = equal.var, ic.type = ic.type,
                   B = ini.B,
                   prob = ini.pi,
                   rho = ini.rho,
                   w = w, control = control,
                   report = report)
      ## check selected ratio
      if(select.ratio != 1) {
        if(mean(tmp$B.hat != 0) >= select.ratio) break
        # selected.var <- sum(apply(tmp$B.hat, 1, function(B.row) {sum(B.row^2) != 0})) - as.numeric(intercept)
        # selected.var <- sum(apply(tmp$B.hat, 1, function(B.row) {sum(B.row^2) != 0})) - as.numeric(intercept)
        # if(selected.var >= p * select.ratio) break
      }
      names(tmp)[which(names(tmp) == "IC")] <- ""
      out.B <- abind(out.B, tmp$B.hat, along = 3)
      out.pi <- cbind(out.pi, tmp$pi.hat)
      out.rho <- cbind(out.rho, tmp$rho.hat)
      out.info <- cbind(out.info, unlist(tail(tmp, 6)))
      lambda.used <- c(lambda.used, lam)
  }
  dimnames(out.B)[[3]] <- colnames(out.pi) <- colnames(out.rho) <-
    paste0("lambda = ", round(out.info[1, ], digits = 4))
  list(lambda = lambda, lambda.used = lambda.used, B.hat = out.B, pi.hat = out.pi, rho.hat = out.rho, IC = out.info, 
       common.only = common.only, common.no.penalty = common.no.penalty, 
       cluster.no.penalty = cluster.no.penalty)
}




##' Finite Mixture Effects Model with Heterogeneity Pursuit
##'
##' Produce solution for specified lambda of regularized finite mixture effects model with lasso 
##' or adaptive lasso; compute the degrees of freeom, likelihood and information criteria (AIC, BIC and GIC) 
##' of the estimators. Model fitting is conducted by EM algorithm and Bregman coordinate descent.
##'
##' The available elements for argument \code{control} include
##' \itemize{
##'    \item{epsilon}: Convergence threshold for generalized EM algorithm.
##'        Defaults value is 1E-6.
##'    
##'    \item{maxit}: Maximum number of passes over the data for all lambda values.
##'        Default is 1000.
##'    
##'    \item{inner.eps}: Convergence threshold for Bregman coordinate descent algorithm.
##'        Defaults value is 1E-6.
##'    
##'    \item{inner.maxit}: Maximum number of iteration for Bregman coordinate descent algorithm.
##'        Defaults value is 200.
##'    
##'    \item{n.ini}: Number of initial values for EM algorithm. Default is 10. In EM algorithm, it is 
##'        preferable to start from several different initial values.
##' }
##'
##' @usage
##' fmrHP(y, X, m, intercept = FALSE, lambda, equal.var = FALSE,
##'       ic.type = c("ALL", "BIC", "AIC", "GIC"),
##'       B = NULL, prob = NULL, rho = NULL, w = NULL,
##'       control = list(), report = FALSE)
##'
##' @param y a vector of response (\eqn{n \times 1})
##' @param X a matrix of covariate (\eqn{n \times p})
##' @param m number of components
##' @param intercept indicating whether intercept should be included
##' @param lambda value of tuning parameter
##' @param equal.var indicating whether variances of different components are equal
##' @param ic.type the information criterion to be used; currently supporting "AIC", "BIC", and "GIC".
##' @param B initial values for the rescaled coefficients with first column being the 
##'     common effect, and the rest \code{m} columns being the heterogeneity for 
##'     corresponding components
##' @param prob initial values for prior probabilitis for different components
##' @param rho initial values for rho vector (\eqn{1 / \sigma}), the reciprocal of standard deviation
##' @param w weight matrix for penalty function. Default option is NULL
##' @param control a list of parameters for controlling the fitting process
##' @param report indicating whether printing the value of objective function during EM algorithm 
##'     for validation checking of initial value.
##'
##' @return
##' A list consisting of
##' \item{y}{vector of response}
##' \item{X}{matrix of covariates}
##' \item{m}{number of components}
##' \item{B.hat}{estimated rescaled coefficient (\eqn{p \times m + 1 \times nlambda})}
##' \item{pi.hat}{estimated prior probabilities (\eqn{m \times nlambda})}
##' \item{rho.hat}{estimated rho values (\eqn{m \times nlambda})}
##' \item{lambda}{lambda used in model fitting}
##' \item{plik}{value of penalized log-likelihood}
##' \item{loglik}{value of log-likelihood}
##' \item{conv}{indicator of convergence of EM algorithm}
##' \item{IC}{values of information criteria}
##' \item{df}{degree of freedom}
##'
##' @examples
##' library(fmerPack)
##' ## problem settings
##' n <- 100; m <- 3; p <- 5;
##' sigma2 <- c(0.1, 0.1, 0.4); rho <- 1 / sqrt(sigma2)
##' phi <- rbind(c(1, 1, 1), c(1, 1, 1), c(0, -3, 3), c(-3, 3, 0), c(3, 0, -3))
##' beta <- t(t(phi) / rho)
##' ## generate response and covariates
##' z <- rmultinom(n, 1, prob= rep(1 / m, m))
##' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
##' y <- MASS::mvrnorm(1, mu = rowSums(t(z) * X[, 1:(nrow(beta))] %*% beta), 
##'                    Sigma = diag(colSums(z * sigma2)))
##' fmrHP(y, X, m = m, lambda = 0.01, control = list(n.ini = 10))
##' @import flexmix glmnet utils
##' @export

fmrHP <- function(y, X, m, intercept = FALSE,
                  lambda, equal.var = FALSE,
                  ic.type = c("ALL", "BIC", "AIC", "GIC"),
                  B = NULL,
                  prob = NULL,
                  rho = NULL,
                  w = NULL,
                  control = list(),
                  report = FALSE) {
  ## input: y, vector of response
  ##        X, matrix of covariates
  ##        B, initial value for beta matrix
  ##        prob, initial value for prior probability
  ##        rho, initial value for 1 / sd
  ##        control, a list of parameters for controlling the fitting process
  if(intercept) {
    X <- cbind(1, X)
  }
  n <- length(y) ## dimension
  p <- ncol(X)
  ic.type <- match.arg(ic.type)
  control <- do.call("fmrHP.control", control)
  epsilon <- control$epsilon
  maxit <- control$maxit
  inner.eps <- control$inner.eps
  inner.maxit <- control$inner.maxit
  mu <- control$mu
  ######################### Set initial values ###############################
  ## weight matrix
  if(is.null(w)) {
    w <- matrix(1, p, m + 1)
  } else {
    ## check any Inf to avoid error
    w[is.infinite(w)] <- 1e6
    if(any(w < 0)) {
      stop("Penalty weight cannot be negative")
    }
    if(nrow(w) != p) {
      stop("Dimensions of weight matrix don't match number of covariates")
    }
  }
  if(intercept) {
    w[1] <- 0
  }
  B.0 <- B
  pi.0 <- prob
  rho.0 <- rho
  if(is.null(B.0) | is.null(pi.0) | is.null(rho.0)) {
    ## compute initial value from flexmix package
    if(report){
      print("setting initial values...")
    }
    fo <- sample(rep(seq(5), length = n))
    if(intercept) {
      ini.mix <- flexmix(y ~ X[, -1], k = m,
                         model = FLXMRglmnet(foldid = fo, adaptive = FALSE, 
                                             intercept = intercept),
                         control = list(iter.max = inner.maxit, minprior = 0))
      sigma <- as.vector(tail(parameters(ini.mix), 1))
      coef <- t(t(head(parameters(ini.mix), -1)) / sigma)
    } else {
      ini.mix <- flexmix(y ~ X, k = m,
                         model = FLXMRglmnet(foldid = fo, adaptive = FALSE, 
                                             intercept = intercept),
                         control = list(iter.max = inner.maxit, minprior = 0))
      sigma <- as.vector(tail(parameters(ini.mix), 1))
      coef <- t(t(head(parameters(ini.mix), -1)[-1, ]) / sigma)
    }
    dimnames(coef) <- NULL
    ## set initial values for beta, pi, rho
    B.0 <- ifelse(is.null(B.0),
                  list(cbind(rowMeans(coef), coef - rowMeans(coef))),
                  list(B.0))[[1]]
    pi.0 <- ifelse(is.null(pi.0),
                   list(prior(ini.mix)), list(pi.0))[[1]]
    rho.0 <- ifelse(is.null(rho.0),
                    list(1 / sigma), list(rho.0))[[1]]
  } else if(!is.null(B.0)) {
    if(nrow(B.0) != p) {
      stop("Dimensions of initial coefficient matrix don't match number of covariates")
    }
  }
  ###################### EM algorithm for estimation #########################
  ## sets initial loop conditions
  delta <- delta.plik <- Inf
  iteration <- 0
  plik <- Inf
  while((delta > epsilon | delta.plik > sqrt(epsilon)) & iteration < maxit) {
    phi <- as.matrix(B.0[, 1] + B.0[, -1])
    ## E-step: calculate p_ij
    pij <- as.matrix(postProb(n = n, m = m, pi_0 = pi.0, rho_0 = rho.0, y = y, X = X, phi = phi))
    ## generalized M step
    ## update pi
    pi.1 <- colMeans(pij, na.rm = TRUE)
    na.Ind <- ! apply(is.na(pij), 1, any)
    ## update rho
    rho.1 <- as.vector(estRho(n = n, m = m, pij = as.matrix(pij[na.Ind, ]), y = y[na.Ind], X = X[na.Ind, ], phi = phi, equal_var = equal.var))
    ## update beta
    B.1 <- bcdaRcpp(y[na.Ind], X[na.Ind, ], n, p, m, B_Start = B.0, pij = as.matrix(pij[na.Ind, ]), rho = rho.1, lambda, w,
                    eps = inner.eps, maxit = inner.maxit, mu = mu)
    delta <- sum(c(as.vector(B.1 - B.0), rho.1 - rho.0, pi.1 - pi.0)^2) /
      (sum(c(as.vector(B.0), rho.0, pi.0)^2) + epsilon)
    B.0 <- B.1
    pi.0 <- pi.1
    rho.0 <- rho.1
    iteration <- iteration + 1
    ## calculate plik
    plikold <- plik
    plik <- plikHP(y, X, rho = rho.0, prob = pi.0, B = B.0, lambda = lambda, w = w)$nplik
    delta.plik <- abs(plik - plikold) / (1 + abs(plik))
    if(report) {
      ## check likelihood
      if(plikold <= plik) {
        warning("error: penalized negative loglik not reduced")
      }
      print(paste0("negative penalized likelihood for iteration ", iteration, ": ", plik))
      print(paste0("delta.plik for iteration ", iteration, ": ", delta.plik))
      print(paste0("delta for iteration ", iteration, ": ", delta))
    }
  }
  ############################ summary of results ############################
  ## convert near zeros to zero
  B.0[abs(B.0) < inner.eps * 10] <- 0
  lik.res <- plikHP(y, X, rho = rho.0, prob = pi.0, B = B.0, lambda = lambda, w = w)
  plik <- lik.res$nplik
  loglik <- lik.res$loglik
  ## compute IC
  fit <- list(
    y = y,
    X = X,
    m = m,
    B.hat = B.0, pi.hat = pi.0, rho.hat = rho.0,
    lambda = lambda,
    plik = plik,
    loglik = loglik,
    conv = delta < epsilon)
  IC <- criteria.fmrHP(fit, criteria = ic.type, equal.var = equal.var)
  return(c(fit, list(IC = IC$IC, df = IC$df)))
}




### internal functions =======================================================
fmrHP.control <- function(epsilon = 1e-6,
                          maxit = 500L,
                          inner.eps = 1e-6,
                          inner.maxit = 200L,
                          n.ini = 10,
                          mu = 1) {
  list(
    epsilon = epsilon,
    maxit = maxit,
    inner.eps = inner.eps,
    inner.maxit = inner.maxit,
    n.ini = n.ini,
    mu = mu
  )
}

fmrHP.modstr <- function(lambda = NULL, lambda.min.ratio = 0.01,
                         nlambda = 100, w = NULL, intercept = FALSE, 
                         common.only = NULL, 
                         common.no.penalty = NULL, cluster.no.penalty = NULL, 
                         select.ratio = 1) {
  list(lambda = lambda, lambda.min.ratio = lambda.min.ratio,
       nlambda = nlambda, w = w, intercept = intercept,
       common.only = common.only, 
       common.no.penalty = common.no.penalty, cluster.no.penalty = cluster.no.penalty, 
       select.ratio = select.ratio)
}

## set initial values if initial values are not provided
ini.EM <- function(y, X, B = NULL, prob = NULL, rho = NULL, n, p, m,
                   n.ini = 10, inner.maxit = 100,
                   report = FALSE, intercept = FALSE) {
  if(is.null(B) | is.null(prob) | is.null(rho)) {
    if(report) {
      cat(paste0("set initial values for ", n.ini, " times\n"))
    }
    ## compute initial value from flexmix package
    fo <- sample(rep(seq(5), length = n))
    ini.mix <- initFlexmix(y ~ X, k = m,
                           model = FLXMRglmnet(foldid = fo, adaptive = FALSE, 
                                               intercept = intercept),
                           control = list(iter.max = inner.maxit, minprior = 0),
                           nrep = n.ini,
                           verbose = report)
    sigma <- as.vector(tail(parameters(ini.mix), 1))
    if(!intercept) {
      coef <- t(t(head(parameters(ini.mix), -1)[-1, ]) / sigma)
    } else {
      coef <- t(t(head(parameters(ini.mix), -1)) / sigma)
    }
    dimnames(coef) <- NULL
    ## set initial values for beta, pi, rho
    B <- ifelse(is.null(B),
                list(cbind(rowMeans(coef), coef - rowMeans(coef))),
                list(B))[[1]]
    prob <- ifelse(is.null(prob),
                   list(prior(ini.mix)), list(prob))[[1]]
    rho <- ifelse(is.null(rho),
                  list(1 / sigma), list(rho))[[1]]
    return(list(B = B, prob = prob, rho = rho))
  } else {
    return(list(B = B, prob = prob, rho = rho))
  }
}

## compute criteria informations for results from fmrHP() function
criteria.fmrHP <- function(fit, criteria = c("ALL", "BIC", "AIC", "GIC"), equal.var = FALSE) {
  criteria <- match.arg(criteria)
  ## import data set
  X <- fit$X
  y <- fit$y
  n <- nrow(X)
  p <- ncol(X)
  m <- fit$m
  lambda <- fit$lambda
  ## get estimated coefficients
  B <- fit$B.hat
  Pi <- fit$pi.hat
  Rho <- fit$rho.hat
  ## compute phi
  Phi <- B[, 1] + B[, -1]
  # compute degree of freedom
  # df <- m + m - 1 + sum(Phi != 0) - (m - 1) * sum(rowSums((B[, -1])^2) == 0 & B[, 1] != 0)
  df <- m + m - 1 + sum(B[, 1] != 0)  + sum(rowSums(B[, -1, drop = FALSE] != 0)[rowSums(B[, -1, drop = FALSE] != 0) > 1] - 1)
  if(equal.var) {
    df <- df - (m - 1)
  }
  IC <- NULL
  log.lik <- sum(log(
    rowSums(
      t(Pi * Rho * t(exp(- (t(Rho) %x% y - X %*% (Phi))^2 / 2) / sqrt(2 * pi)))
    )))
  if(criteria == "ALL") {
    IC <- -2 * log.lik + c(2, log(n), log(m * p + m + m - 1) * log(log(n))) * df
    names(IC) <- c("AIC", "BIC", "GIC")
  } else if(criteria == "AIC") {
    IC <- -2 * log.lik + 2 * df
    names(IC) <- "AIC"
  } else if(criteria == "BIC") {
    IC <- -2 * log.lik + log(n) * df
    names(IC) <- "BIC"
  } else if(criteria == "GIC") {
    IC <- -2 * log.lik + log(log(n)) * log(m * p + m + m - 1) * df
    names(IC) <- "GIC"
  } else {
    stop("unknow selection criteria")
  }
  return(list(IC = IC, lambda = lambda, df = df))
}

## compute penalized likelihood for given estimated paramaters
plikHP <- function(y, X, prob, rho, B, lambda, w, fmrHP = TRUE) {
  if(fmrHP == TRUE) {
    phi <- B[, 1] + B[, -1]
  } else {
    phi <- B
  }
  n <- length(y)
  ## compute density within each components
  loglik <- sum(log(colSums(prob * rho * t(exp(- (t(rho) %x% y - X %*% phi)^2 / 2)) / sqrt(2*pi))))
  nplik <- - loglik + sum(n * lambda * w * abs(B))
  return(list(loglik = loglik, nplik = nplik))
}

## compute the maximun of lambda such that all estimated coefficients are equal to 0
lambdamax <- function(y, X){
  # x: design matrix
  # y: dependent variable
  n <- length(y)
  rho <- sqrt(n / sum((y - mean(y))^{2}))
  mu <- mean(y)*rho
  grad <- - rho * crossprod(y, X) + mu * n * colMeans(X)
  max(abs(grad)) / n
}