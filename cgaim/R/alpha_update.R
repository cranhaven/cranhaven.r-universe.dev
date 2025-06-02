################################################################################
#
# Main function updating alphas at the current iteration
#
################################################################################

alpha_update <- function(y, x, w, index, dgz, alpha,
  delta = TRUE, Cmat = NULL, bvec = NULL, solver, ctol, qp_pars)
{
  d <- ncol(x)
  lvec <- as.vector(table(index))
  # prepare predictors
  zerod <- apply(dgz, 2, function(x) all(x == 0))
  if (any(zerod)) dgz[,zerod] <- .Machine$double.eps
  dgz <- dgz[,index]
  Vmat <- x * dgz
  # prepare response
  if (!delta){ 
    y <- y + Vmat %*% alpha
  }
  # Normal matrices
  wvqr <- qr(Vmat * sqrt(w))
  Rmat <- qr.R(wvqr)
  effects <- qr.qty(wvqr, y * sqrt(w))
  Dmat <- crossprod(Rmat)
  dvec <- drop(crossprod(effects[seq_len(d)], Rmat))
  # Fit
  if (!is.null(Cmat)){
    if (delta) bvec <- bvec + drop(-(Cmat %*% alpha))
    bvec <- bvec + ctol
    # bvec <- bvec + max(c(formals(osqp::osqpSettings)$eps_abs, qp_pars$eps_abs))
    res <- do.call(sprintf("update_%s", solver), 
      list(Dmat = Dmat, dvec = dvec, Cmat = Cmat, bvec = bvec, 
        qp_pars = qp_pars))
    res$active[((Cmat %*% res$alpha) - bvec) <= ctol] <- TRUE
  } else {
    if (wvqr$rank < d){ # For rank deficient cases
      ridge.apply <- MASS::lm.ridge(y ~ 0 + Vmat, weights = w, 
        lambda = seq(0,1,0.01))
      alpha.up <- stats::coef(ridge.apply)[which.min(ridge.apply$GCV),]
    } else {
      # alpha.up <- stats::coef(stats::lm(y ~ 0 + Vmat, weights = w))
      alpha.up <- qr.coef(wvqr, y)
    }
    res <- list(alpha = alpha.up, active = rep(FALSE, d))
  }
  return(res)
}


