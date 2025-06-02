nested_Xsq_nr <- function(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                          step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                          max.score.diff.iter, S.space.H0, tol.psi, tol, max.iter, cut.off, delta) {
  mph.fit_H0 <- mph.fit(y, h.fct = h0.fct, h.mean = TRUE, strata = strata, fixed.strata = fixed.strata,
                        maxiter = max.mph.iter, step = step, change.step.after = change.step.after,
                        y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                        norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                        derht.fct = h0.fct.deriv)
  cons.MLE.m_H0 <- mph.fit_H0$m   # m^hat_0
  S0.fct.m_H0 <- S0.fct(cons.MLE.m_H0)   # S0(m^hat_0)
  cov.cons.MLE.m_H0 <- mph.fit_H0$covm   # avar(m^hat_0)
  # avar(S0(m^hat_0)) = (partial S0(m) / partial m')|_{m = m^hat_0} *
  #                      avar(m^hat_0) * (partial S0(m)' / partial m)|_{m = m^hat_0}.
  # pp. 366 of Lang (2004), when S0(.) is Z-homogeneous this is true. It is not true in general.
  if (!is.null(S0.fct.deriv)) {
    avar.S0.fct.m_H0 <- t(S0.fct.deriv(cons.MLE.m_H0)) %*% cov.cons.MLE.m_H0 %*% S0.fct.deriv(cons.MLE.m_H0)
  }
  else {
    # numerical derivative
    partial_S0_partial_m <- num.deriv.fct(S0.fct, cons.MLE.m_H0)
    avar.S0.fct.m_H0 <- t(partial_S0_partial_m) %*% cov.cons.MLE.m_H0 %*% partial_S0_partial_m
  }
  if (avar.S0.fct.m_H0 < 0 & avar.S0.fct.m_H0 > -1e-9) {
    # because of numerical derivative used
    avar.S0.fct.m_H0 <- 0
  }
  ase.S0.fct.m_H0 <- c(sqrt(avar.S0.fct.m_H0))     # ase(S0(m^hat_0))
  epsilon <- ase.S0.fct.m_H0 / 2
  nested.Xsq.CI.high <- NULL
  nested.Xsq.CI.low <- NULL
  if (!is.null(S.space.H0)) {
    if (length(S.space.H0) == 2) {
      lower.boundary <- S.space.H0[[1]]
      upper.boundary <- S.space.H0[[2]]
    }
    else {
      # S.space.H0 is composed of at least two disjoint intervals.
      for (s_f_index in seq(1, length(S.space.H0) / 2)) {
        s_f_lower_temp <- S.space.H0[s_f_index][[1]]
        s_f_upper_temp <- S.space.H0[s_f_index][[2]]
        if ((S0.fct.m_H0 >= s_f_lower_temp) & (S0.fct.m_H0 <= s_f_upper_temp)) {
          lower.boundary <- s_f_lower_temp
          upper.boundary <- s_f_upper_temp
          break
        }
      }
    }
    if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
      nested.Xsq.CI.high <- upper.boundary
    }
    if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
      nested.Xsq.CI.low <- lower.boundary
    }
  }
  # compute nested.Xsq.CI.high
  if (is.null(nested.Xsq.CI.high)) {
    psi.high.list <- rep(NA, max.iter + 3)
    f.psi.high.list <- rep(NA, max.iter + 3)
    # psi.0, psi.1, psi.2, ..., psi.(max.iter+2)
    psi.high.list[c(1, 2, 3)] <- S0.fct.m_H0 + epsilon * c(1, 2, 3)
    epsilon.new <- epsilon
    if (!is.null(S.space.H0)) {
      while (psi.high.list[3] >= upper.boundary) {
        epsilon.new <- epsilon.new * 0.9
        psi.high.list[c(1, 2, 3)] <- S0.fct.m_H0 + epsilon.new * c(1, 2, 3)
      }
    }
    f.psi.high.list[1] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                psi = psi.high.list[1], max.mph.iter = max.mph.iter, step = step,
                                change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                cons.MLE.m_H0 = cons.MLE.m_H0)
    while (f.psi.high.list[1] > cut.off) {
      epsilon.new <- epsilon.new / 4
      psi.high.list[c(1, 2, 3)] <- S0.fct.m_H0 + epsilon.new * c(1, 2, 3)
      f.psi.high.list[1] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                  h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                  S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                  psi = psi.high.list[1], max.mph.iter = max.mph.iter, step = step,
                                  change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                  norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                  max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                  cons.MLE.m_H0 = cons.MLE.m_H0)
    }
    f.psi.high.list[2] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                psi = psi.high.list[2], max.mph.iter = max.mph.iter, step = step,
                                change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                cons.MLE.m_H0 = cons.MLE.m_H0)
    epsilon.new.shrink <- 1
    while (f.psi.high.list[2] > cut.off) {
      epsilon.new.shrink <- epsilon.new.shrink / 3
      psi.high.list[c(2, 3)] <- psi.high.list[1] + epsilon.new * epsilon.new.shrink * c(1, 2)
      f.psi.high.list[2] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                  h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                  S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                  psi = psi.high.list[2], max.mph.iter = max.mph.iter, step = step,
                                  change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                  norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                  max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                  cons.MLE.m_H0 = cons.MLE.m_H0)
    }
    f.psi.high.list[3] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                psi = psi.high.list[3], max.mph.iter = max.mph.iter, step = step,
                                change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                cons.MLE.m_H0 = cons.MLE.m_H0)
    high.first.step.quad.coeff <- quadratic.fit(psi.high.list[c(1, 2, 3)], f.psi.high.list[c(1, 2, 3)])
    c1 <- min(f.psi.high.list[3] + delta, cut.off)
    psi.high.list[4] <- max(solve_quadratic(high.first.step.quad.coeff[1], high.first.step.quad.coeff[2],
                                            high.first.step.quad.coeff[3] - c1)[[2]])
    if (!is.null(S.space.H0)) {
      if (psi.high.list[4] >= upper.boundary) {
        psi.high.list[4] <- upper.boundary - tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
      }
      else if (psi.high.list[4] <= lower.boundary) {
        psi.high.list[4] <- lower.boundary + tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
      }
    }
    f.psi.high.list[4] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                psi = psi.high.list[4], max.mph.iter = max.mph.iter, step = step,
                                change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                cons.MLE.m_H0 = cons.MLE.m_H0)
    iter.count <- 1
    while (iter.count < max.iter) {
      iter.count <- iter.count + 1
      high.step.quad.coeff <- quadratic.fit(psi.high.list[seq(iter.count, iter.count+2)],
                                            f.psi.high.list[seq(iter.count, iter.count+2)])
      c <- min(f.psi.high.list[iter.count+2] + delta, cut.off)
      psi.high.list[iter.count+3] <- max(solve_quadratic(high.step.quad.coeff[1], high.step.quad.coeff[2],
                                                         high.step.quad.coeff[3] - c)[[2]])
      if (!is.null(S.space.H0)) {
        if (psi.high.list[iter.count+3] >= upper.boundary) {
          psi.high.list[iter.count+3] <- upper.boundary - tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
        }
        else if (psi.high.list[iter.count+3] <= lower.boundary) {
          psi.high.list[iter.count+3] <- lower.boundary + tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
        }
      }
      f.psi.high.list[iter.count+3] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                             h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                             S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                             psi = psi.high.list[iter.count+3], max.mph.iter = max.mph.iter, step = step,
                                             change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                             norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                             max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                             cons.MLE.m_H0 = cons.MLE.m_H0)
      if ((abs(f.psi.high.list[iter.count+3] - cut.off) < tol)  |
          (abs(psi.high.list[iter.count+3] - psi.high.list[iter.count+2]) < tol.psi)) {
        break
      }
    }
    nested.Xsq.CI.high <- psi.high.list[iter.count+3]
  }
  # compute nested.Xsq.CI.low
  if (is.null(nested.Xsq.CI.low)) {
    psi.low.list <- rep(NA, max.iter + 3)
    f.psi.low.list <- rep(NA, max.iter + 3)
    psi.low.list[c(1, 2, 3)] <- S0.fct.m_H0 - epsilon * c(1, 2, 3)
    epsilon.new <- epsilon
    if (!is.null(S.space.H0)) {
      while (psi.low.list[3] <= lower.boundary) {
        epsilon.new <- epsilon.new * 0.9
        psi.low.list[c(1, 2, 3)] <- S0.fct.m_H0 - epsilon.new * c(1, 2, 3)
      }
    }
    f.psi.low.list[1] <- f.psi(y, strata = strata, fixed.strata = fixed.strata,
                               h0.fct = h0.fct, h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                               S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                               psi = psi.low.list[1], max.mph.iter = max.mph.iter, step = step,
                               change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                               cons.MLE.m_H0 = cons.MLE.m_H0)
    while (f.psi.low.list[1] > cut.off) {
      epsilon.new <- epsilon.new / 4
      psi.low.list[c(1, 2, 3)] <- S0.fct.m_H0 - epsilon.new * c(1, 2, 3)
      f.psi.low.list[1] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                 h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                 S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                 psi = psi.low.list[1], max.mph.iter = max.mph.iter, step = step,
                                 change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                 norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                 max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                 cons.MLE.m_H0 = cons.MLE.m_H0)
    }
    f.psi.low.list[2] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                               h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                               S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                               psi = psi.low.list[2], max.mph.iter = max.mph.iter, step = step,
                               change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                               cons.MLE.m_H0 = cons.MLE.m_H0)
    epsilon.new.shrink <- 1
    while (f.psi.low.list[2] > cut.off) {
      epsilon.new.shrink <- epsilon.new.shrink / 3
      psi.low.list[c(2, 3)] <- psi.low.list[1] - epsilon.new * epsilon.new.shrink * c(1, 2)
      f.psi.low.list[2] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                 h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                 S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                 psi = psi.low.list[2], max.mph.iter = max.mph.iter, step = step,
                                 change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                 norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                 max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                 cons.MLE.m_H0 = cons.MLE.m_H0)
    }
    f.psi.low.list[3] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                               h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                               S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                               psi = psi.low.list[3], max.mph.iter = max.mph.iter, step = step,
                               change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                               cons.MLE.m_H0 = cons.MLE.m_H0)
    low.first.step.quad.coeff <- quadratic.fit(psi.low.list[c(1, 2, 3)], f.psi.low.list[c(1, 2, 3)])
    c1 <- min(f.psi.low.list[3] + delta, cut.off)
    psi.low.list[4] <- min(solve_quadratic(low.first.step.quad.coeff[1], low.first.step.quad.coeff[2],
                                           low.first.step.quad.coeff[3] - c1)[[2]])
    if (!is.null(S.space.H0)) {
      if (psi.low.list[4] <= lower.boundary) {
        psi.low.list[4] <- lower.boundary + tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
      }
      else if (psi.low.list[4] >= upper.boundary) {
        psi.low.list[4] <- upper.boundary - tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
      }
    }
    f.psi.low.list[4] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                               h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                               S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                               psi = psi.low.list[4], max.mph.iter = max.mph.iter, step = step,
                               change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                               cons.MLE.m_H0 = cons.MLE.m_H0)
    iter.count <- 1
    while (iter.count < max.iter) {
      iter.count <- iter.count + 1
      low.step.quad.coeff <- quadratic.fit(psi.low.list[seq(iter.count, iter.count+2)],
                                           f.psi.low.list[seq(iter.count, iter.count+2)])
      c <- min(f.psi.low.list[iter.count+2] + delta, cut.off)
      psi.low.list[iter.count+3] <- min(solve_quadratic(low.step.quad.coeff[1], low.step.quad.coeff[2],
                                                        low.step.quad.coeff[3] - c)[[2]])
      if (!is.null(S.space.H0)) {
        if (psi.low.list[iter.count+3] <= lower.boundary) {
          psi.low.list[iter.count+3] <- lower.boundary + tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
        }
        else if (psi.low.list[iter.count+3] >= upper.boundary) {
          psi.low.list[iter.count+3] <- upper.boundary - tol.psi * (upper.boundary - lower.boundary) / 2 * runif(1, 0, 1)
        }
      }
      f.psi.low.list[iter.count+3] <- f.psi(y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                            h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct,
                                            S0.fct.deriv = S0.fct.deriv, method_specific = "nested.Xsq",
                                            psi = psi.low.list[iter.count+3], max.mph.iter = max.mph.iter, step = step,
                                            change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                                            norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                                            max.score.diff.iter = max.score.diff.iter, pdlambda = NULL,
                                            cons.MLE.m_H0 = cons.MLE.m_H0)
      if ((abs(f.psi.low.list[iter.count+3] - cut.off) < tol) |
          (abs(psi.low.list[iter.count+3] - psi.low.list[iter.count+2]) < tol.psi)) {
        break
      }
    }
    nested.Xsq.CI.low <- psi.low.list[iter.count+3]
  }
  nested.Xsq.CI <- sort(c(nested.Xsq.CI.low, nested.Xsq.CI.high))
  t(nested.Xsq.CI)
}



