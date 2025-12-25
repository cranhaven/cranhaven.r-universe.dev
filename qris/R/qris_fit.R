#' A wrapper for the different qris fit
#'
#' This version is more flexible for future extension
#' 
#' @noRd
qris.fit <- function(info, method) {
  switch(method,
         smooth = qris.smooth(info),
         iterative = qris.iter(info),
         nonsmooth = qris.nonsmooth(info))
}

qris.nonsmooth <- function(info) {
  ## 1. : L1-minimization : Estimating equation for estimating beta (using rq)
  out <- with(info, {
    M <- 1e6
    pseudo1 <- -colSums(X*I*W)
    pseudo2 <- 2 * colSums(X*I*Q)
    Y.reg <- c(data[,2], M, M)
    X.reg <- rbind(X, rbind(pseudo1, pseudo2))
    W.reg <- c(I * W, 1, 1)
    Li.fit <- rq.wfit(X.reg, Y.reg, weights = W.reg)
    coefficient <- as.vector(Li.fit$coefficients)
    uTime <- sort(unique(data$Z))
    if (all(Li.fit$coefficients <= 10)){
      if (nB <= 1) {
        se <- rep(NA_real_, nc)
        vcov <- matrix(NA_real_, nc, nc)
        return(list(coefficient = coefficient, stderr = se, vcov = vcov))
      }
      if (se == "fmb"){
        fmb.result <- matrix(NA_real_, nc, nB)
        for (j in 1:nB){
          ## generating perturbation variable
          eta <- rexp(n)
          if (all(data$delta == 1)) {
            W_star <- rep(1, n)
          } else {
            survp <- drop(ghatC(data$Z, 1 - data$delta, eta))
            ghatstart0 <- ifelse(t0 > 0, survp[findInterval(t0, uTime)], 1)
            W_star <- data$delta / survp[findInterval(data$Z, uTime)] * ghatstart0
            W_star[is.na(W_star)] <- max(W_star, na.rm = TRUE)
          }
          rev_pseudo1 <- -colSums(X * W_star * eta)
          rev_pseudo2 <- 2 * colSums(X * I * eta * Q)
          rev_Y.reg <- c(data[,2], M, M)
          rev_X.reg <- rbind(X, rbind(rev_pseudo1, rev_pseudo2))
          rev_W.reg <- c(W_star * I * eta, 1, 1)
          fmb.fit <- rq.wfit(rev_X.reg, rev_Y.reg, weights = rev_W.reg)
          if (all(fmb.fit$coef <= 10)){
            fmb.result[, j] <- as.vector(fmb.fit$coef)
          } 
        }
        fmb.sigma <- try(cov(t(fmb.result), use = "complete.obs"), silent = T)
        if(class(fmb.sigma)[1] == "try-error" | sum(!is.na(colSums(fmb.result))) <= 1){
          stop("More resampling iterations are necessary")
        } else {
          fmb.se <- sqrt(diag(fmb.sigma))
          out <- list(coefficient = coefficient, stderr = fmb.se, vcov = fmb.sigma)
        }
      } else {
        stop("Only full multiplier bootstrapping (fmb) is available for nonsmooth estimating equation approach")
      }
    } else {
      coefficient <- se <- rep(NA, nc)
      vcov <- matrix(NA, nc, nc)
      out <- list(coefficient = coefficient, stderr = se, vcov = vcov)
    }})
  out
}

qris.iter <- function(info) {
  out <- with(info, {
    new_h <- old_h <- H
    new_sigma <- old_sigma <- H*n
    iter_beta_result <- iter_SE_result <- matrix(NA_real_, control$maxiter, nc)
    iter_beta_result[1,] <- old_beta <- new_beta <- betastart
    iter_SE_result[1,] <- sqrt(diag(new_sigma))
    iter_norm_result <- rep(NA_real_, control$maxiter)
    uTime <- sort(unique(data$Z))
    if (se == "fmb") {
      for (k in 1:control$maxiter){
        old_beta <- new_beta
        old_sigma <- new_sigma
        old_h <- new_h
        slope_a <- Amat(old_beta, X, W, old_h, I, logZ, Q) / n
        ## Step 1 : Update beta()
        ## Singular matrix 'a' error message and break
        if (qr(slope_a)$rank < min(dim(slope_a))) {
          warning("'A' matrix is singular during iteration. Please try the non-iterative method.")
          break
        } else {
          new_beta <- old_beta + qr.solve(slope_a) %*% (isObj(old_beta, X, W, old_h, I, logZ, Q)/n)
          iter_beta_result[k,] <- t(new_beta)
          ## Step 2 : Update Sigma()
          result.fmb <- matrix(NA_real_, nc, nB)
          for (j in 1:nB){
            ## generating perturbation variable
            eta <- rexp(n)
            if (all(data$delta == 1)) {
              W_star <- rep(1, n)
            } else {
              survp <- drop(ghatC(data$Z, 1 - data$delta, eta))
              ghatstart0 <- ifelse(t0 > 0, survp[findInterval(t0, uTime)], 1)
              W_star <- data$delta / survp[findInterval(data$Z, uTime)] * ghatstart0
              W_star[is.na(W_star)] <- max(W_star, na.rm = TRUE)
            }
            fmb.fit <- nleqslv(old_beta, function(b)
              rev_isObj(b, X, W_star, old_h, eta, I, logZ, Q)/n)
            if (fmb.fit$termcd == 1 | fmb.fit$termcd == 2) {
              result.fmb[, j] <- fmb.fit$x
            }
          }
          new_sigma <- try(cov(t(result.fmb), use = "complete.obs"), silent = T)
          ## Trace the result
          if (verbose) {
            cat("\n Step:", k)
            cat("\n beta:", as.numeric(new_beta))
            cat("\n se:", as.numeric(sqrt(diag(new_sigma))), "\n")
          }
          if (qr(new_sigma)$rank < min(dim(new_sigma))) {
            warning("Futher fmb method is inapplicable to this dataset. Please try other estimation methods")
            new_sigma <- old_sigma
            break
          } else {
            new_h <- new_sigma
            iter_SE_result[k,] <- sqrt(diag(new_sigma))
            iter_norm_result[k] <- norm(new_beta-old_beta, "F")
            if(iter_norm_result[k]>=100*iter_norm_result[1]) {
              warning("Point estimation failed to converge.")
              break
            }
            if(norm(new_beta-old_beta, "i") < control$tol) break
          }
        }
      } ## end for loop
    } else {
      for (k in 1:control$maxiter){
        old_beta <- new_beta
        old_sigma <- new_sigma
        old_h <- new_h
        slope_a <- Amat(old_beta, X, W, old_h, I, logZ, Q)/n
        ## Step 1 : Update beta()
        ## Singular matrix 'a' error message and break
        if (qr(slope_a)$rank < min(dim(slope_a))) {
          warning("'A' matrix is singular during iteration. Please try the non-iterative method.")
          break
        } else {
          new_beta <- old_beta + qr.solve(slope_a) %*% (isObj(old_beta, X, W, old_h, I, logZ, Q)/n)
          iter_beta_result[k,] <- t(new_beta)
          ## Step 2 : Update Sigma()
          result.pmb <- matrix(NA, nc, nB)
          m2 <- isObjL(old_beta, X, H, logZ)
          for (j in 1:nB){
            ## generating perturbation variable
            eta <- rexp(n)
            if (all(data$delta == 1)) {
              W_star <- rep(1, n)
            } else {
              survp <- drop(ghatC(data$Z, 1 - data$delta, eta))
              ghatstart0 <- ifelse(t0 > 0, survp[findInterval(t0, uTime)], 1)
              W_star <- data$delta / survp[findInterval(data$Z, uTime)] * ghatstart0
              W_star[is.na(W_star)] <- max(W_star, na.rm = TRUE)
            }
            result.pmb[,j] <- t(X * I * eta) %*% (m2 * W_star - Q) / n
            ## rev_isObj(old_beta, X, W_star, old_h, eta, I, logZ, Q)/n           
          }
          new_V <- cov(t(result.pmb), use = "complete.obs")
          new_sigma <- t(qr.solve(slope_a)) %*% new_V %*% qr.solve(slope_a)
          new_h <- new_sigma
          ## Trace the result
          if (verbose) {
            cat("\n beta:", as.numeric(new_beta), "\n")
            cat("\n se:", as.numeric(sqrt(diag(new_sigma))), "\n")
          }
          iter_SE_result[k,] <- sqrt(diag(new_sigma))
          iter_norm_result[k] <- norm(new_beta-old_beta, "F")
          if(iter_norm_result[k]>=100*iter_norm_result[1]) {
            warning("Point estimation result is diverging")
            break
            }
          if(norm(new_beta-old_beta, "i") < control$tol) break
        }
      } ## end for loop
    }
    iter_beta_result <- iter_beta_result[!is.na(iter_beta_result[,1]),,drop = FALSE]
    iter_SE_result <- iter_SE_result[!is.na(iter_SE_result[,1]),,drop = FALSE]
    iter_norm_result <- iter_norm_result[!is.na(iter_norm_result)]
    out <- list(coefficient = as.numeric(tail(iter_beta_result, n = 1)),
                trace.coefficient = iter_beta_result,
                stderr = as.numeric(tail(iter_SE_result, n = 1)),
                trace.stderr = iter_SE_result,
                vcov = new_sigma, iterno = k,
                norm = iter_norm_result)   
  })
  out
}

qris.smooth <- function(info) {
  out <- with(info, {
    uTime <- sort(unique(data$Z))
    smooth.fit <- nleqslv(betastart, function(b) isObj(b, X, W, H, I, logZ, Q))
    if (smooth.fit$termcd == 1 | smooth.fit$termcd == 2) {
      coefficient <- smooth.fit$x
      if (nB <= 1) {
        se <- rep(NA, nc)
        vcov <- matrix(NA, nc, nc)
        return(list(coefficient = coefficient, stderr = se, vcov = vcov))
      }
      if (se == "fmb"){
        ## Full multiplier bootstrap
        smooth.fmb.result <- matrix(NA, nc, nB)
        for (j in 1:nB){
          ## generating perturbation variable
          eta <- rexp(n)
          if (all(data$delta == 1)) {
            W_star <- rep(1, n)
          } else {
            survp <- drop(ghatC(data$Z, 1 - data$delta, eta))
            ghatstart0 <- ifelse(t0 > 0, survp[findInterval(t0, uTime)], 1)
            W_star <- data$delta / survp[findInterval(data$Z, uTime)] * ghatstart0
            W_star[is.na(W_star)] <- max(W_star, na.rm = TRUE)
          }
          fmb.fit <- nleqslv(coefficient, function(b) rev_isObj(b, X, W_star, H, eta, I, logZ, Q))
          if (fmb.fit$termcd == 1 | fmb.fit$termcd == 2) {
            smooth.fmb.result[,j] <- fmb.fit$x
          } 
        }
        fmb.sigma <- try(cov(t(smooth.fmb.result), use = "complete.obs"), silent = T)
        if(class(fmb.sigma)[1] == "try-error" | sum(!is.na(colSums(smooth.fmb.result))) <= 1){
          stop("More resampling iterations are needed")
        } else {
          fmb.se <- sqrt(diag(fmb.sigma))
          out <- list(coefficient = coefficient, stderr = fmb.se, vcov = fmb.sigma)
        }
      } else if (se == "pmb") {
        ## Partial Multiplier Bootstrap
        smooth.pmb.result <- isObjE(coefficient, X, H, I, logZ, data$delta, t0, Q, nB)
        pmb.v <- try(cov(t(smooth.pmb.result), use = "complete.obs"), silent = T)
        pmb.a <- Amat(coefficient, X, W, H, I, logZ, Q) / n
        ## Singular matrix 'a' error message and break
        if (qr(pmb.a)$rank < min(dim(pmb.a))) {
          pmb.se <- rep(NA,nc+1)
          stop("'A' matrix is singular during iteration. Please try the nonsmooth method.")
        } else {
          pmb.inva <- qr.solve(pmb.a)
          pmb.sigma <- t(pmb.inva) %*% pmb.v %*% pmb.inva
          pmb.se <- sqrt(diag(pmb.sigma))
        }
        out <- list(coefficient=coefficient, stderr = pmb.se, vcov = pmb.sigma)
      } else {
        stop("Select either 'fmb' or 'pmb'")
      }
    } else {
      coefficient <- se <- rep(NA, nc)
      vcov <- matrix(NA, nc, nc)
      out <- list(coefficient = coefficient, stderr = se, vcov = vcov)
    }})
  out
}
