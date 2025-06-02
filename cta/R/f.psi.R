f.psi <- function(y, strata, fixed.strata, h0.fct, h0.fct.deriv = NULL, S0.fct, S0.fct.deriv = NULL,
                  method_specific, psi, max.mph.iter, step, change.step.after, y.eps, iter.orig,
                  norm.diff.conv, norm.score.conv, max.score.diff.iter, pdlambda = NULL,
                  Gsq_H0, Xsq_H0, PD_H0, cons.MLE.m_H0) {
  # A general function for computing (only) one of the test statistics.
  # The test statistics include:
  # the difference in G^2 statistic, G^2(psi) - G^2 = G^2(y; H0(psi)) - G^2(y; H0);
  # the difference in X^2 statistic, X^2(psi) - X^2 = X^2(y; H0(psi)) - X^2(y; H0);
  # the difference in power-divergence statistic, with specified pdlambda,
  #                                      PD(psi) - PD = PD(y; H0(psi)) - PD(y; H0);
  # the nested G^2 statistic, G^2(y; H0(psi) | H0);
  # the nested X^2 statistic, X^2(y; H0(psi) | H0);
  # the nested power-divergence statistic, with specified pdlambda, PD(y; H0(psi) | H0).
  # Note that only one of Gsq_H0, Xsq_H0, PD_H0, or cons.MLE.m_H0 should be specified.
  if (is.null(pdlambda)) {
    if (method_specific == "diff.Gsq") {
      pdlambda <- 0
      PD_H0 <- Gsq_H0
    }
    else if (method_specific == "diff.Xsq") {
      pdlambda <- 1
      PD_H0 <- Xsq_H0
    }
    else if (method_specific == "nested.Gsq") {
      pdlambda <- 0
    }
    else if (method_specific == "nested.Xsq") {
      pdlambda <- 1
    }
  }
  if (!is.function(h0.fct)) {
    # h0.fct = 0
    h0.psi.fct <- function(m, h0.fct, S0.fct, psi) {
      S0.fct(m) - psi
    }
    if (!is.null(S0.fct.deriv)) {
      h0.psi.fct.deriv <- function(m, h0.fct.deriv, S0.fct.deriv) {
        S0.fct.deriv(m)
      }
      mph.fit_H0psi <- mph.fit(y = y, h.fct = function(m) h0.psi.fct(m, h0.fct = h0.fct, S0.fct = S0.fct, psi = psi),
                               h.mean = TRUE, strata = strata, fixed.strata = fixed.strata, maxiter = max.mph.iter,
                               step = step, change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = pdlambda,
                               derht.fct = function(m) h0.psi.fct.deriv(m, h0.fct.deriv = h0.fct.deriv, S0.fct.deriv = S0.fct.deriv))
    }
    else {
      h0.psi.fct.deriv <- NULL
      mph.fit_H0psi <- mph.fit(y = y, h.fct = function(m) h0.psi.fct(m, h0.fct = h0.fct, S0.fct = S0.fct, psi = psi),
                               h.mean = TRUE, strata = strata, fixed.strata = fixed.strata, maxiter = max.mph.iter,
                               step = step, change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = pdlambda, derht.fct = NULL)
    }
  }
  else {
    h0.psi.fct <- function(m, h0.fct, S0.fct, psi) {
      rbind(h0.fct(m), S0.fct(m) - psi)
    }
    if (!is.null(h0.fct.deriv) & !is.null(S0.fct.deriv)) {
      h0.psi.fct.deriv <- function(m, h0.fct.deriv, S0.fct.deriv) {
        # partial h0*(m)' / partial m, where h0*(m) = [h0(m); S0(m) - psi].
        cbind(h0.fct.deriv(m), S0.fct.deriv(m))
      }
      mph.fit_H0psi <- mph.fit(y = y, h.fct = function(m) h0.psi.fct(m, h0.fct = h0.fct, S0.fct = S0.fct, psi = psi),
                               h.mean = TRUE, strata = strata, fixed.strata = fixed.strata, maxiter = max.mph.iter,
                               step = step, change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = pdlambda,
                               derht.fct = function(m) h0.psi.fct.deriv(m, h0.fct.deriv = h0.fct.deriv, S0.fct.deriv = S0.fct.deriv))
    }
    else {
      h0.psi.fct.deriv <- NULL
      mph.fit_H0psi <- mph.fit(y = y, h.fct = function(m) h0.psi.fct(m, h0.fct = h0.fct, S0.fct = S0.fct, psi = psi),
                               h.mean = TRUE, strata = strata, fixed.strata = fixed.strata, maxiter = max.mph.iter,
                               step = step, change.step.after = change.step.after, y.eps = y.eps, iter.orig = iter.orig,
                               norm.diff.conv = norm.diff.conv, norm.score.conv = norm.score.conv,
                               max.score.diff.iter = max.score.diff.iter, pdlambda = pdlambda, derht.fct = NULL)
    }
  }
  if (any(method_specific %in% c("diff.Gsq", "diff.Xsq", "diff.PD"))) {
    PD_H0psi <- mph.fit_H0psi$PD.stat
    as.numeric(PD_H0psi - PD_H0)
  }
  else {
    # nested versions
    cons.MLE.m_H0psi <- as.vector(mph.fit_H0psi$m)
    # Compute PD(y; H0(psi) | H0)
    if ((pdlambda != 0) & (pdlambda != -1)) {
      nested.PD.stat <- 2 * sum(cons.MLE.m_H0*((cons.MLE.m_H0/cons.MLE.m_H0psi)^pdlambda-1))/pdlambda/(pdlambda+1)
    }
    else {
      if (pdlambda == 0) {
        nested.PD.stat <- 2 * sum(cons.MLE.m_H0 * log(cons.MLE.m_H0 / cons.MLE.m_H0psi))
      }
      if (pdlambda == -1) {
        cons.MLE.m_H0.mod <- cons.MLE.m_H0
        cons.MLE.m_H0.mod[cons.MLE.m_H0.mod == 0] <- 1e-3
        nested.PD.stat <- 2 * sum(cons.MLE.m_H0psi*log(cons.MLE.m_H0psi/cons.MLE.m_H0.mod))
      }
    }
    nested.PD.stat
  }
}

