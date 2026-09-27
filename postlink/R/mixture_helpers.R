#' @import stats
#' @noRd
constrained_logistic_regression <- function(X, y, bound, cmax.iter) {
 X <- as.matrix(X)
 d <- ncol(X)
 barx <- colMeans(X)
 tau <- 0.1
 
 nloglik_logistic <- function(mu) {
  cutoff <- 1E-10
  flag0 <- (mu < cutoff)
  flag1 <- (mu > 1 - cutoff)
  flag <- flag0 | flag1
  nloglik_nonflag <- sum(-(y[!flag] * log(mu[!flag]) + (1 - y[!flag]) * log(1 - mu[!flag])))
  
  if (any(flag0) && max(y[flag0]) > cutoff) {
   warning("Fitted probabilities zero with non-matching response; evaluation may be inaccurate.")
  }
  if (any(flag1) && min(y[flag1]) < 1 - cutoff) {
   warning("Fitted probabilities one with non-matching response; evaluation may be inaccurate.")
  }
  return(nloglik_nonflag)
 }
 
 grad_Hess <- function(mu) {
  grad <- -crossprod(X, y - mu)
  w <- sqrt(mu * (1 - mu))
  Xw <- sweep(X, MARGIN = 1, STATS = w, FUN = "*")
  Hess <- crossprod(Xw)
  return(list(grad = grad, Hess = Hess))
 }
 
 betacur <- c(bound, rep(0, d - 1))
 mu <- plogis(X %*% betacur)
 iter <- 1
 maxiter <- cmax.iter
 tol <- 1E-8
 objs <- numeric(maxiter)
 objs[1] <- nloglik_logistic(mu)
 
 while (iter < maxiter) {
  gH <- grad_Hess(mu)
  gr <- gH$grad
  H <- gH$Hess
  rhs <- -gr + (H %*% betacur)
  betanew <- solve(H, rhs)
  eta_tmp <- X %*% betanew
  
  if (mean(eta_tmp) > bound) {
   betanew_theta <- solve(rbind(cbind(H, barx), c(barx, 0)), c(rhs, bound))
   betanew <- betanew_theta[1:d]
  }
  
  m <- 0
  upd <- betanew - betacur
  munew <- plogis(X %*% betanew)
  objnew <- nloglik_logistic(munew)
  
  while ((objs[iter] - objnew) < tau * (1/2)^m * sum(upd * (-gr))) {
   m <- m + 1
   if ((1/2)^m < tol^2) {
    betanew <- betacur
    break
   } else {
    betanew <- betacur + upd * (1/2)^m
    munew <- plogis(X %*% betanew)
    objnew <- nloglik_logistic(munew)
   }
  }
  betacur <- betanew
  mu <- munew
  iter <- iter + 1
  objs[iter] <- objnew
  if (iter >= 2 && objs[iter - 1] - objs[iter] < tol) break
 }
 return(list(beta = betacur, objs = objs))
}

#' @noRd
calc_breslow <- function(y, status, weights, linear_pred) {
  # Standard Breslow Estimator for baseline hazard
  ord <- order(y)
  y_sorted <- y[ord]
  status_sorted <- status[ord]
  weights_sorted <- weights[ord]
  
  risk_score <- weights_sorted * exp(linear_pred[ord])
  
  risk_by_time <- rowsum(risk_score, y_sorted, reorder = FALSE)
  events_by_time <- rowsum(status_sorted * weights_sorted, y_sorted, reorder = FALSE)
  unique_times <- sort(unique(y_sorted))
  
  cum_risk <- rev(cumsum(rev(risk_by_time)))
  haz_inc <- events_by_time / cum_risk
  cum_haz <- cumsum(haz_inc)
  
  diff_times <- diff(unique_times)
  diff_haz <- diff(cum_haz)
  diff_times[diff_times == 0] <- 1e-10
  
  dens <- c(cum_haz[1], diff_haz / diff_times)
  list(times = unique_times, cumhaz = cum_haz, dens = dens)
}