compute_cons_MLE_ase <- function(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv,
                                 max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv,
                                 norm.score.conv, max.score.diff.iter) {
  #
  # This program computes the constrained MLE of S0(m) and its associated approximate standard error,
  # subject to the equality constraints h0(m) = 0 under the specified strata and fixed.strata configuration.
  # Here m is the vector of expected cell counts, i.e. m = E(Y).
  #
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
  c(S0.fct.m_H0, ase.S0.fct.m_H0)
}
