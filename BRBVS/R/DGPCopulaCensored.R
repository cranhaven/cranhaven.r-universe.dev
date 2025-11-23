#' Data Generation using Copula for Mixed Censoring
#'
#' This function simulates data using a copula-based approach for mixed censoring scenarios.
#' It is designed to generate data for two survival times with potential censoring and covariates.
#'
#' @param n Integer. The number of observations to be generated.
#' @param Sigma Matrix. A pp.relxpp.rel covariance matrix used for multivariate normal distribution simulation.
#' @param Beta.coef List. A list containing coefficients for the linear predictors.
#'        Expected elements are: beta11, beta12, beta21, beta22, beta31, beta32, and beta33.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{dataFull}: A data frame with all the generated variables including true survival times, censoring times, covariates, and censoring indicators.
#'     \item \code{dataSim}: A subset of \code{dataFull} containing only the simulated survival times, covariates, and censoring indicators.
#'   }
#' @noRd
datagenCopulaMixCens <- function(n, Sigma ,Beta.coef){

  rMVN <- function(...) GJRM::rMVN(...)

  # Scenario Di simulatione A
  cor.cov <- Sigma
  cov <- rMVN(n, rep(0,3), cor.cov)
  cov <- stats::pnorm(cov)
  z1  <- cov[, 1]
  z2  <- cov[, 2]
  z3  <- cov[, 3]

  ## No smooth for the moment
  #s11 <- function(x) sin(2*pi*x)
  #s21 <- function(x) -0.2*exp(3.2 * x) #0.5 * sin(3*pi*x) #-0.2*exp(3.2 * x)  #NEW NO SMOOTH
  #s31 <- function(x) 3 * sin(pi * x)

  beta11 = Beta.coef$beta11
  beta12= Beta.coef$beta12
  beta21 = Beta.coef$beta21
  beta22= Beta.coef$beta22
  beta31 =Beta.coef$beta31
  beta32= Beta.coef$beta32
  beta33=Beta.coef$beta33

  ###########
  ###########

  f1 <- function(t, beta1, beta2,sm.fn, u, z1, z2){



    S_0 <- 0.9 * exp(-0.4*t^2.5) + 0.1*exp(-0.1*t^1)


    exp( -exp(log(-log(S_0)) + beta1*z1 + beta2*z2) ) - u

  }

  # same baseline and same PO, may try same basaline but PH and PO


  f2 <- function(t, beta1,beta2 ,sm.fn, u, z1, z3){



    S_0 <- 0.9 * exp(-0.4*t^2.5) + 0.1*exp(-0.1*t^1)

    1/(1 + exp(log((1-S_0)/S_0) + beta1*z1 + beta2*z3 )) - u  #NEW NO SMOOTH




  }

  ###########
  ###########


  u    <- stats::runif(n, 0, 1)
  t    <- rep(NA, n)

  for (i in 1:n){
    t[i] <- stats::uniroot(f1, c(0, 8), tol = .Machine$double.eps^0.5,
                    beta1 = beta11, beta2=beta12, u = u[i],
                    z1 = z1[i], z2 = z2[i],  extendInt = "yes")$root
  }


  c1 <-      stats::runif(n, 0, 2)
  c2 <- c1 + stats::runif(n, 0, 6)

  dataSim <- data.frame(t.true1 = t, c11 = c1, c12 = c2, t11 = NA, t12 = NA, z1, z2, z3, cens = character(n),
                        surv1 = u, stringsAsFactors = FALSE)


  for (i in 1:n){

    if(t[i] > c2[i]){
      dataSim$t11[i] <- c2[i]
      dataSim$t12[i] <- NA # redundant but nice for clarity
      dataSim$cens[i] <- "R"}

    else {
      dataSim$t11[i] <- t[i]
      dataSim$t12[i] <- NA
      dataSim$cens[i] <- "U"

    }

  }
  #p=0.25 high
  #p=0.60 mild censoring



  ###

 # eta.theta <- beta31*z1 + beta32*z2 + beta33*z3
  # this is for Clayton
  eta.theta <- 1.2
  theta     <- exp(eta.theta)

  ###

  u2      <- stats::runif(n, 0, 1)
  u_prime <- ((u2^(-theta/(1 + theta)) - 1) * u^(-theta) + 1)^(-1/theta)
  t       <- rep(NA, n)

  for (i in 1:n){
    t[i] <- stats::uniroot(f2, c(0, 8), tol = .Machine$double.eps^0.5,
                    beta1 = beta21,beta2=beta22 ,  u = u_prime[i],
                    z1 = z1[i],  z3 = z3[i], extendInt = "yes")$root
  }

  dataSim$t.true2 = t

  c1 <-      stats::runif(n, 0, 2)
  c2 <- c1 + stats::runif(n, 0, 6)

  dataSim$c21 = c1
  dataSim$c22 = c2

  for (i in 1:n){

    if(t[i] > c2[i]){
      dataSim$t21[i] <- c2[i]
      dataSim$t22[i] <- NA
      dataSim$cens[i] <- paste(dataSim$cens[i], "R", sep = "")
    }
     else {
       dataSim$t21[i] <- t[i]
       dataSim$t22[i] <- NA
       dataSim$cens[i] <- paste(substr(dataSim$cens[i], 1, 1), "U", sep = "")

    }

  }




  ##

  dataSim$surv2 = u_prime
  dataSim$surv.joint = u2

  list(dataFull = dataSim,
       dataSim = dataSim[, c('t11', 't12', 't21', 't22',
                             'z1', 'z2', 'z3', 'cens')])


}

