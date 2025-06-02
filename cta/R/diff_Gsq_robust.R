diff_Gsq_robust <- function(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                            step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                            max.score.diff.iter, S.space.H0, tol.psi, tol, max.iter, cut.off, delta,
                            adj.epsilon, iter.robust.max, iter.robust.eff) {
  r <- tryCatch(diff_Gsq_nr(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                            step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                            max.score.diff.iter, S.space.H0, tol.psi, tol, max.iter, cut.off, delta),
                error = function(e) {t(c(NA, NA))})
  if (all(is.na(r))) {
    warning_this <- "diff.Gsq.CI: Adjustment used. Not on original data.\n"
    # robustifying procedure
    adj.epsilon.plus <- adj.epsilon * seq(1, iter.robust.max)
    diff.Gsq.CI_all <- matrix(NA, nrow = iter.robust.max, ncol = 2)
    iter.robust <- 1
    iter.eff <- 0
    while (iter.robust <= iter.robust.max & iter.eff < iter.robust.eff) {
      r_temp <- tryCatch(diff_Gsq_nr(y + adj.epsilon.plus[iter.robust], strata, fixed.strata, h0.fct, h0.fct.deriv,
                                     S0.fct, S0.fct.deriv, max.mph.iter, step, change.step.after, y.eps, iter.orig,
                                     norm.diff.conv, norm.score.conv, max.score.diff.iter, S.space.H0, tol.psi,
                                     tol, max.iter, cut.off, delta),
                         error = function(e) {t(c(NA, NA))})
      diff.Gsq.CI_all[iter.robust, ] <- r_temp
      iter.robust <- iter.robust + 1
      if (!any(is.na(r_temp))) {
        iter.eff <- iter.eff + 1
      }
    }
    diff.Gsq.CI_all <- cbind(adj.epsilon.plus, diff.Gsq.CI_all)
    diff.Gsq.CI_all <- as.matrix(na.omit(as.data.frame(diff.Gsq.CI_all)))
    if (nrow(diff.Gsq.CI_all) == 0) {
      list(t(c(NA, NA)), warning_this)
    }
    else {
      if (nrow(diff.Gsq.CI_all) <= 4) {
        diff.Gsq.CI_low <- lm(diff.Gsq.CI_all[, 2] ~ poly(diff.Gsq.CI_all[, 1], 1, raw = TRUE))[[1]][1]
        diff.Gsq.CI_high <- lm(diff.Gsq.CI_all[, 3] ~ poly(diff.Gsq.CI_all[, 1], 1, raw = TRUE))[[1]][1]
      }
      else {
        if (nrow(diff.Gsq.CI_all) <= 7) {
          diff.Gsq.CI_low <- lm(diff.Gsq.CI_all[, 2] ~ poly(diff.Gsq.CI_all[, 1], 2, raw = TRUE))[[1]][1]
          diff.Gsq.CI_high <- lm(diff.Gsq.CI_all[, 3] ~ poly(diff.Gsq.CI_all[, 1], 2, raw = TRUE))[[1]][1]
        }
        else {
          diff.Gsq.CI_low <- lm(diff.Gsq.CI_all[, 2] ~ poly(diff.Gsq.CI_all[, 1], 3, raw = TRUE))[[1]][1]
          diff.Gsq.CI_high <- lm(diff.Gsq.CI_all[, 3] ~ poly(diff.Gsq.CI_all[, 1], 3, raw = TRUE))[[1]][1]
        }
      }
      list(t(sort(c(diff.Gsq.CI_low, diff.Gsq.CI_high))), warning_this)
    }
  }
  else {
    list(r, NULL)
  }
}




