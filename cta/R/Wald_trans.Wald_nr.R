Wald_trans.Wald_nr <- function(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                               step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                               max.score.diff.iter, cut.off, S.space.H0, trans.g, trans.g.deriv, trans.g.inv) {
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
  # Compute Wald.CI
  ase.S0.fct.m_H0 <- c(sqrt(avar.S0.fct.m_H0))     # ase(S0(m^hat_0))
  Wald.CI <- S0.fct.m_H0 + sqrt(cut.off) * ase.S0.fct.m_H0 * c(-1, 1)
  # End Wald.CI
  # Compute trans.Wald.CI
  if (!is.null(trans.g)) {
    g.S0.fct.m_H0 <- trans.g(S0.fct.m_H0)    # g(S0(m^hat_0))
    # S0(.) is zero-order Z-homogeneous, then g(S(.)) is also zero-order Z-homogeneous.
    # avar(g(S0(m^hat_0))) = [(d g(w) / d w)|_{w = S0(m^hat_0)}]^2 * avar(S0(m^hat_0)).
    if (!is.null(trans.g.deriv)) {
      avar.g.S0.fct.m_H0 <- (trans.g.deriv(S0.fct.m_H0))^2 * avar.S0.fct.m_H0
    }
    else {
      # numerical derivative
      d_g_d_w <- grad(trans.g, S0.fct.m_H0)
      avar.g.S0.fct.m_H0 <- d_g_d_w^2 * avar.S0.fct.m_H0
    }
    ase.g.S0.fct.m_H0 <- c(sqrt(avar.g.S0.fct.m_H0))      # ase(g(S0(m^hat_0)))
    trans.Wald.CI <- trans.g.inv(g.S0.fct.m_H0 + sqrt(cut.off) * ase.g.S0.fct.m_H0 * c(-1, 1))
    # End trans.Wald.CI
    t(cbind(Wald.CI, trans.Wald.CI))
  }
  else {
    t(Wald.CI)
  }
}


