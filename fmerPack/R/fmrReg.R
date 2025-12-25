##' Finite Mixture Model with lasso and adaptive penalty
##'
##' Produce solution paths of regularized finite mixture model with lasso or 
##' adaptive lasso penalty; compute the degrees of freeom, likelihood 
##' and information criteria (AIC, BIC and GIC) of the estimators. 
##' Model fitting is conducted by EM algorithm and coordinate descent.
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
##'    \item{no.penalty}: A vector of user-specified indicators of the variables
##'        with no penalty.
##'    
##'    \item{common.var}: A vector of user-specified indicators of the variables
##'        with common effect among different components.
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
##'    \item{inner.maxit}: Maximum number of iteration for flexmix package to compute initial values.
##'        Defaults value is 200.
##'    
##'    \item{n.ini}: Number of initial values for EM algorithm. Default is 10. In EM algorithm, it is 
##'        preferable to start from several different initial values.
##' }
##'
##' @usage
##' path.fmrReg(y, X, m, equal.var = FALSE,
##'             ic.type = "ALL", B = NULL, prob = NULL, rho = NULL, 
##'             control = list(), modstr = list(), report = FALSE)
##'
##' @param y a vector of response (\eqn{n \times 1})
##' @param X a matrix of covariate (\eqn{n \times p})
##' @param m number of components
##' @param equal.var indicating whether variances of different components are equal
##' @param ic.type the information criterion to be used; currently supporting "ALL", "AIC", "BIC", and "GIC".
##' @param B initial values for the rescaled coefficients with columns being 
##'     the columns being the coefficient for different components
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
##' \item{B.hat}{estimated rescaled coefficient (\eqn{p \times m \times nlambda})}
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
##' fit1 <- path.fmrReg(y, X, m = m, modstr = list(nlambda = 10), control = list(n.ini = 1))
##' ## adaptive lasso
##' fit2 <- path.fmrReg(y, X, m = m, 
##'                    modstr = list(w = abs(select.tuning(fit1)$B + 1e-6)^2))
##' }
##' @import flexmix glmnet
##' @importFrom abind abind
##' @export

path.fmrReg <- function(y, X, m, equal.var = FALSE, 
                        ic.type = "ALL",
                        B = NULL, 
                        prob = NULL, 
                        rho = NULL, 
                        control = list(),
                        modstr = list(), 
                        report = FALSE) {
  Call <- match.call()
  ## set control values for model fitting
  control <- do.call("fmrReg.control", control)
  n <- length(y)
  p <- ncol(X)
  ## set mod strings
  modstr <- do.call("fmrReg.modstr", modstr)
  lambda <- modstr$lambda
  lambda.min.ratio <- modstr$lambda.min.ratio
  nlambda <- modstr$nlambda
  select.ratio <- modstr$select.ratio
  ## intercept?
  intercept <- modstr$intercept
  w <- modstr$w
  if(is.null(w)) {
    w <- matrix(1, p, m)
    if(intercept) {
      w <- rbind(rep(0, m), w)
    }
  } else {
    w <- as.matrix(w)
    ## check any Inf to avoid error
    w[is.infinite(w)] <- 1e6
    if(any(w < 0)) {
      stop("weight cannot be negative")
    }
    if(nrow(w) != p + as.numeric(intercept)) {
      stop("Dimensions of weight matrix don't match number of covariates")
    }
  }
  ## check the user-specific penalty options: no.penalty
  #### variables without penalty and with only common effect
  no.penalty <- modstr$no.penalty
  common.var <- modstr$common.var
  if(! is.null(no.penalty)) {
    if(! (is.vector(no.penalty) && length(no.penalty) <= p)) {
      stop(paste0("The no.penalty parameter needs to be a vector with dimension not greater than ", p))
    } else {
      w[no.penalty + as.numeric(intercept), ] <- 0
    }
  }
  if(! is.null(common.var)) {
    if(! (is.vector(common.var) && length(common.var) <= p)) {
      stop(paste0("The common.var parameter needs to be a vector with dimension not greater than ", p))
    }
  }
  ## set initial values, if some of the values exist, then keep them
  ini.mix <- ini.REM(y = y, X = X, B = B, prob = prob, rho = rho, n = n, p = p, m = m, 
                    inner.maxit = control$inner.maxit, 
                    n.ini = control$n.ini, 
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
      tmp <- fmrReg(y, X, m, intercept = intercept,
                    lambda = lam, equal.var = equal.var, common.var = common.var, 
                    ic.type = ic.type, 
                    B = ini.B, 
                    prob = ini.pi, 
                    rho = ini.rho,
                    w = w, control = control, 
                    report = report)
      ## check selected ratio
      if(select.ratio != 1) {
        if(mean(tmp$B.hat != 0) >= select.ratio) break
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
       no.penalty = no.penalty, common.var = common.var)
}





##' Finite Mixture Model with lasso and adaptive penalty
##'
##' Produce solution for specific lambda of regularized finite mixture model 
##' with lasso or adaptive lasso penalty; compute the degrees of freeom, 
##' likelihood and information criteria (AIC, BIC and GIC) of the estimators.
##' Model fitting is conducted by EM algorithm and coordinate descent.
##'
##' The available elements for argument \code{control} include
##' \itemize{
##'    \item{epsilon}: Convergence threshold for generalized EM algorithm.
##'        Defaults value is 1E-6.
##'    
##'    \item{maxit}: Maximum number of passes over the data for all lambda values.
##'        Default is 1000.
##' 
##'    \item{inner.maxit}: Maximum number of iteration for flexmix package to compute initial values.
##'        Defaults value is 200.
##'    
##'    \item{n.ini}: Number of initial values for EM algorithm. Default is 10. In EM algorithm, it is 
##'        preferable to start from several different initial values.
##' }
##' 
##' @usage
##' fmrReg(y, X, m, intercept = FALSE, lambda, equal.var = FALSE, common.var = NULL,
##'        ic.type = c("ALL", "BIC", "AIC", "GIC"), 
##'        B = NULL, prob = NULL, rho = NULL, w = NULL, 
##'        control = list(), report = FALSE)
##'
##' @param y a vector of response (\eqn{n \times 1})
##' @param X a matrix of covariate (\eqn{n \times p})
##' @param m number of components
##' @param intercept indicating whether intercept should be included
##' @param lambda value of tuning parameter
##' @param equal.var indicating whether variances of different components are equal
##' @param common.var indicating whether the effects over different components are the same for specific covariates
##' @param ic.type the information criterion to be used; currently supporting "AIC", "BIC", and "GIC".
##' @param B initial values for the rescaled coefficients with columns being the 
##'     coefficients for different components
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
##' \item{B.hat}{estimated rescaled coefficient (\eqn{p \times m \times nlambda})}
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
##' fmrReg(y, X, m = m, lambda = 0.01, control = list(n.ini = 10))
##' @import flexmix glmnet utils
##' @export

fmrReg <- function(y, X, m, intercept = FALSE,
                   lambda, equal.var = FALSE, 
                   common.var = NULL,
                   ic.type = c("ALL", "BIC", "AIC", "GIC"), 
                   B = NULL,
                   prob = NULL,
                   rho = NULL,
                   w = NULL,
                   control = list(),
                   report = FALSE) {
  ## get dimensions for sample size and number of covariates
  if(intercept) {
    X <- cbind(1, X)
  }
  n <- length(y)
  p <- ncol(X)
  if(any(w < 0)) {
    stop("Penalty weight cannot be negative")
  }
  if(! is.null(common.var)) {
    if(! (is.vector(common.var) && length(common.var) <= p)) {
      stop(paste0("The common.var parameter needs to be a vector with dimension not greater than ", p))
    } else {
      common.var <- common.var + as.numeric(intercept)
    }
  }
  ## change the common.var according to the intercept
  ic.type <- match.arg(ic.type)
  control <- do.call("fmrReg.control", control)
  epsilon <- control$epsilon
  maxit <- control$maxit
  inner.maxit <- control$inner.maxit
  ######################### Set initial values ###############################
  ## weight matrix
  if(is.null(w)) {
    w <- matrix(1, p, m)
  } else {
    w <- as.matrix(w)
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
    w[1, ] <- 0
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
                  list(coef),
                  list(B.0))[[1]]
    pi.0 <- ifelse(is.null(pi.0),
                   list(prior(ini.mix)), list(pi.0))[[1]]
    rho.0 <- ifelse(is.null(rho.0),
                    list(1 / sigma), list(rho.0))[[1]]
  } else if(!is.null(B.0)) {
    B.0 <- as.matrix(B.0)
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
    ## E-step: calculate p_ij
    pij <- t(pi.0 * rho.0 * t(exp(- (t(rho.0) %x% y - X %*% (B.0))^2 / 2)))
    pij <- as.matrix(pij / rowSums(pij))
    ## generalized M step
    ## update pi
    pi.1 <- colMeans(pij, na.rm = TRUE)
    na.Ind <- ! apply(is.na(pij), 1, any)
    ## update rho
    ytXt <- diag(crossprod(pij[na.Ind, ] * y[na.Ind], X[na.Ind, ] %*% B.0))
    yy <- colSums(as.matrix(pij[na.Ind, ]) * y[na.Ind]^2)
    if(!equal.var) {
      ## assuming unequal variances
      rho.1 <- (ytXt + sqrt(ytXt^2 + 4 *  yy * colSums(pij, na.rm = TRUE))) / (2 * yy)
    } else {
      ## assuming equal variance
      rho.1 <- rep((sum(ytXt) + sqrt(sum(ytXt)^2 + 4 *  sum(yy) * sum(pij, na.rm = TRUE))) /
                     (2 * sum(yy)), m)
    }
    ## update beta
    B.1 <- cdaFMR(y[na.Ind], X[na.Ind, ], m, B.Start = B.0, pij = as.matrix(pij[na.Ind, ]),
                  rho = rho.1, lambda, w, common.var = common.var)

    ## convergence?
    delta <- sum(c(as.vector(B.1 - B.0), rho.1 - rho.0, pi.1 - pi.0)^2) /
      (sum(c(as.vector(B.0), rho.0, pi.0)^2) + epsilon)
    # delta <- max(abs(c(as.vector(B.1 - B.0), rho.1 - rho.0, pi.1 - pi.0)) /
    #                (abs(c(as.vector(B.0), rho.0, pi.0)) + epsilon))

    B.0 <- B.1
    pi.0 <- pi.1
    rho.0 <- rho.1
    iteration <- iteration + 1
    ## calculate plik
    plikold <- plik
    plik <- plikHP(y, X, rho = rho.0, prob = pi.0, B = B.0, lambda = lambda, w = w, fmrHP = FALSE)$nplik
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
  lik.res <- plikHP(y, X, rho = rho.0, prob = pi.0, B = B.0, lambda = lambda, w = w, fmrHP = FALSE)
  plik <- lik.res$nplik
  loglik <- lik.res$loglik
  ## compute IC
  df <- m + m - 1 + sum(B.0 != 0)
  if(ic.type == "ALL") {
    IC <- -2 * loglik + c(2, log(n), log(m + m - 1 + m * p) * log(log(n))) * df
    names(IC) <- c("AIC", "BIC", "GIC")
  } else if(ic.type == "AIC") {
    IC <- -2 * loglik + 2 * df
    names(IC) <- "AIC"
  } else if(ic.type == "BIC") {
    IC <- -2 * loglik + log(n) * df
    names(IC) <- "BIC"
  } else if(ic.type == "GIC") {
    IC <- -2 * loglik + log(log(n)) * log(m + m - 1 + m * p) * df
    names(IC) <- "GIC"
  } else {
    stop("unknow selection criteria")
  }
  ## return results
  list(y = y, 
       X = X, 
       m = m, 
       B.hat = B.0, pi.hat = pi.0, rho.hat = rho.0, 
       lambda = lambda, 
       plik = plik, 
       loglik = loglik, 
       conv = delta < epsilon, 
       IC = IC, df = df)
}




## internal functions ========================================================

cdaFMR <- function(y, X, m, B.Start, pij, rho, lambda, w, common.var) {
  n <- nrow(X)
  p <- ncol(X)
  ## coordinate descent algorithm with just one replicates
  Bs.0 <- B.Start
  Bs.1 <- Bs.0
  
  ## update over common covariates
  for(k in common.var) {
      t <- NULL
      xx.common <- NULL
      for(j in 1:m) {
        xtilde <- sqrt(pij[, j]) * X
        xx <- crossprod(xtilde)
        ytilde <- sqrt(pij[, j]) * y
        yx <- crossprod(ytilde, xtilde)
        Bs.1[k, ] <- 0
        t <- c(t, rho[j] * yx[k] - sum(xx[k, ] * Bs.1[, j]))
        xx.common <- c(xx.common, xx[k, k])
      }
      t <- mean(t)
      xx.common <- mean(xx.common)
      if(t > n * mean(w[k, ]) * lambda) {
        Bs.1[k, ] <- (t - n * mean(w[k, ]) * lambda) / (xx.common)
      } else if (t < - n * mean(w[k, ]) * lambda) {
        Bs.1[k, ] <- (t + n * mean(w[k, ]) * lambda) / (xx.common)
      }
  }
  
  ## update over different covariates
  for(j in 1:m) {
    xtilde <- sqrt(pij[, j]) * X
    xx <- crossprod(xtilde)
    ytilde <- sqrt(pij[, j]) * y
    yx <- crossprod(ytilde, xtilde)
    ## update over p covariates
    for (k in (1:p)[! 1:p %in% common.var]) {
      if(k %in% common.var) {
        next
      }
      Bs.1[k, j] <- 0
      ## update beta0
      t <- rho[j] * yx[k] - sum(xx[k, ] * Bs.1[, j])
      if(t > n * w[k, j] * lambda) {
        Bs.1[k, j] <- (t - n * w[k, j] * lambda) / (xx[k, k])
      } else if (t < - n * w[k, j] * lambda) {
        Bs.1[k, j] <- (t + n * w[k, j] * lambda) / (xx[k, k])
      }
    }
  }
  return(Bs.1)
}

ini.REM <- function(y, X, B = NULL, prob = NULL, rho = NULL, n, p, m,
                    n.ini = 10, inner.maxit = 100, report = FALSE, 
                    intercept = FALSE) {
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
    ## set initial values for \bbeta, \pi, \rho
    B <- ifelse(is.null(B), 
                list(coef), 
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

fmrReg.control <- function(epsilon = 1e-6,
                           maxit = 1000L,
                           inner.maxit = 200L, 
                           n.ini = 10) {
  list(
    epsilon = epsilon,
    maxit = maxit,
    inner.maxit = inner.maxit, 
    n.ini = n.ini
  )
}

fmrReg.modstr <- function(lambda = NULL, lambda.min.ratio = 0.01, 
                          nlambda = 100, w = NULL, intercept = FALSE,
                          no.penalty = NULL, common.var = NULL,
                          select.ratio = 1) {
  list(lambda = lambda, lambda.min.ratio = lambda.min.ratio, 
       nlambda = nlambda, w = w, intercept = intercept, 
       no.penalty = no.penalty, common.var = common.var,
       select.ratio = select.ratio)
}