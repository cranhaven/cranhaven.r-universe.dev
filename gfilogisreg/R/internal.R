#' @useDynLib gfilogisreg
#' @importFrom Rcpp evalCpp
NULL

isone <- function(x){
  abs(x-1) < 0.1
}

#' @importFrom stats runif qlogis plogis
#' @noRd
rtlogis1 <- function(b){
  out <- qlogis(runif(1L, min = 0, max = plogis(b)))
  if(out == -Inf){
    if(b > qlogis(1e-16))
      out <- runif(1L, qlogis(1e-16), b)
    else
      out <- b
  }
  out
}
rtlogis2 <- function(b){
  out <- qlogis(runif(1L, min = plogis(b), max = 1))
  if(out == Inf){
    if(b < qlogis(1e-16, lower.tail = FALSE))
      out <- runif(1L, b, qlogis(1e-16, lower.tail = FALSE))
    else
      out <- b
  }
  out
}

# logit <- function(u) log(u/(1-u)) # = qlogis
# dlogit <- function(u) 1 / (u*(1-u))
# expit <- function(x)  1 / (1+exp(-x)) # = plogis
#
# ldlogit <- function(u) -log(u) - log1p(-u)
# ldlogis <- function(x) x - 2*log1p(exp(x))
# dldlogis <- function(x) 1 - 2*expit(x)

# #' @importFrom BB BBoptim
# #' @importFrom stats dlogis runif
# #' @noRd
# rcd <- function(n, P, b, B){
#   d <- length(B)
#   f <- function(uv){
#     vecx <- c(P %*% logit(uv) + b)
#     prod(dlogis(vecx)) * prod(dlogit(uv))
#   }
#   # logf <- function(uv){
#   #   vecx <- c(P %*% logit(uv) + b)
#   #   sum(ldlogis(vecx)) + sum(ldlogit(uv))
#   # }
#   # grl_i <- function(uv, i){
#   #   vecx <- c(P %*% logit(uv) + b)
#   #   dlogit(uv[i]) * sum(P[, i] * dldlogis(vecx)) + (2*uv[i]-1)/(uv[i]*(1-uv[i]))
#   # }
#   # grl <- function(uv){
#   #   vapply(1L:d, function(i) grl_i(uv, i), numeric(1L))
#   # }
#   # gr <- function(uv){
#   #   f(uv) * grl(uv)
#   # }
#   # eps <- .Machine$double.eps
#   # # umax ####
#   # opt <- BBoptim(
#   #   par = expit(c(B)),
#   #   fn = logf,
#   #   gr = grl,
#   #   lower = rep(eps, d),
#   #   upper = rep(1-eps, d),
#   #   control = list(
#   #     maximize = TRUE,
#   #     trace = FALSE,
#   #     checkGrad = FALSE,
#   #     maxit = 10000,
#   #     maxfeval = 100000
#   #   ),
#   #   quiet = TRUE
#   # )
#   # if(opt[["convergence"]] != 0){
#   #   stop(
#   #     sprintf(
#   #       "Convergence not achieved (umax) - code: %d.",
#   #       opt[["convergence"]]
#   #     )
#   #   )
#   # }
#   # mu <- opt[["par"]]
#   # umax <- (exp(opt[["value"]]))^(2/(d+2))
#   # # vmin ####
#   # vmin <- numeric(d)
#   # for(i in 1L:d){
#   #   opt <- BBoptim(
#   #     par = `[<-`(rep(0.5, d), i, mu[i]/2),
#   #     fn = function(uv) -(logf(uv) + (d+2) * log(mu[i] - uv[i])),
#   #     gr = function(uv) -grl(uv) + (d+2) * `[<-`(numeric(d), i, 1/(mu[i] - uv[i])),
#   #     lower = rep(eps, d),
#   #     upper = `[<-`(rep(1, d), i, mu[i]) - eps,
#   #     control = list(
#   #       maximize = FALSE,
#   #       trace = FALSE,
#   #       checkGrad = FALSE,
#   #       maxit = 10000,
#   #       maxfeval = 100000
#   #     ),
#   #     quiet = TRUE
#   #   )
#   #   if(opt[["convergence"]] != 0){
#   #     stop(
#   #       sprintf(
#   #         "Convergence not achieved (vmin) - code: %d.",
#   #         opt[["convergence"]]
#   #       )
#   #     )
#   #   }
#   #   vmin[i] <- -exp(-opt[["value"]]/(d+2))
#   # }
#   # # vmax ####
#   # vmax <- numeric(d)
#   # for(i in 1L:d){
#   #   opt <- BBoptim(
#   #     par = `[<-`(rep(0.5, d), i, (mu[i]+1)/2),
#   #     fn = function(uv) logf(uv) + (d+2) * log(uv[i] - mu[i]),
#   #     gr = function(uv) grl(uv) - (d+2) * `[<-`(numeric(d), i, 1/(mu[i] - uv[i])),
#   #     lower = `[<-`(numeric(d), i, mu[i]) + eps,
#   #     upper = rep(1-eps, d),
#   #     control = list(
#   #       maximize = TRUE,
#   #       trace = FALSE,
#   #       checkGrad = FALSE,
#   #       maxit = 10000,
#   #       maxfeval = 100000
#   #     ),
#   #     quiet = TRUE
#   #   )
#   #   if(opt[["convergence"]] != 0){
#   #     stop(
#   #       sprintf(
#   #         "Convergence not achieved (vmax) - code: %d.",
#   #         opt[["convergence"]]
#   #       )
#   #     )
#   #   }
#   #   vmax[i] <- exp(opt[["value"]]/(d+2))
#   # }
#
#   bounds <- routmp::get_bounds(P, b)
#   umax <- bounds$umax
#   mu <- c(bounds$mu)
#   #print(mu) # TODO: check always corner - not really
#   vmin <- c(bounds$vmin)
#   vmax <- c(bounds$vmax)
#   # simulations
#   sims <- matrix(NA_real_, nrow = n, ncol = d)
#   k <- 0L
#   while(k < n){
#     u <- runif(1L, 0, umax)
#     v <- runif(d, vmin, vmax)
#     x <- v/sqrt(u) + mu
#     if(all(x > 0) && all(x < 1) && u < f(x)^(2/(d+2))){
#       k <- k + 1L
#       sims[k, ] <- x
#     }
#   }
#   logit(sims)
# }


# #' @importFrom Runuran ur vnrou.new
# #' @importFrom stats dlogis
# #' @noRd
# rcd <- function(n, P, b, B){
#   d <- length(B)
#   logit(ur(
#     unr = vnrou.new(
#       dim = d,
#       pdf = function(u) prod(dlogis(c(P %*% logit(u) + b))) * prod(dlogit(u)),
#       center = expit(c(B)),
#       ll = rep(0, d), ur = rep(1, d)
#     ),
#     n = n
#   ))
# }
