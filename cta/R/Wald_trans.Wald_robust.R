Wald_trans.Wald_robust <- function(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                                   step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                                   max.score.diff.iter, cut.off, S.space.H0, trans.g, trans.g.deriv, trans.g.inv,
                                   adj.epsilon, iter.robust.max, iter.robust.eff) {
  if (is.null(trans.g)) {
    # Only Wald.CI needs to be computed.
    r <- tryCatch(Wald_trans.Wald_nr(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                                     step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                                     max.score.diff.iter, cut.off, S.space.H0, trans.g, trans.g.deriv, trans.g.inv),
                  error = function(e) {t(c(NA, NA))})
    if (all(is.na(r))) {
      warning_Wald <- "Wald.CI: Adjustment used. Not on original data.\n"
      # robustifying procedure
      adj.epsilon.plus <- adj.epsilon * seq(1, iter.robust.max)
      Wald.CI_all <- matrix(NA, nrow = iter.robust.max, ncol = 2)
      iter.robust <- 1
      iter.eff <- 0
      while (iter.robust <= iter.robust.max & iter.eff < iter.robust.eff) {
        r_temp <- tryCatch(Wald_trans.Wald_nr(y + adj.epsilon.plus[iter.robust], strata, fixed.strata, h0.fct, h0.fct.deriv,
                                              S0.fct, S0.fct.deriv, max.mph.iter, step, change.step.after, y.eps, iter.orig,
                                              norm.diff.conv, norm.score.conv, max.score.diff.iter, cut.off, S.space.H0,
                                              trans.g, trans.g.deriv, trans.g.inv),
                           error = function(e) {t(c(NA, NA))})
        Wald.CI_all[iter.robust, ] <- r_temp
        iter.robust <- iter.robust + 1
        if (!any(is.na(r_temp))) {
          iter.eff <- iter.eff + 1
        }
      }
      Wald.CI_all <- cbind(adj.epsilon.plus, Wald.CI_all)
      Wald.CI_all <- as.matrix(na.omit(as.data.frame(Wald.CI_all)))
      if (nrow(Wald.CI_all) == 0) {
        list(t(c(NA, NA)), warning_Wald)
      }
      else {
        if (nrow(Wald.CI_all) <= 4) {
          Wald.CI_low <- lm(Wald.CI_all[, 2] ~ poly(Wald.CI_all[, 1], 1, raw = TRUE))[[1]][1]
          Wald.CI_high <- lm(Wald.CI_all[, 3] ~ poly(Wald.CI_all[, 1], 1, raw = TRUE))[[1]][1]
        }
        else {
          if (nrow(Wald.CI_all) <= 7) {
            Wald.CI_low <- lm(Wald.CI_all[, 2] ~ poly(Wald.CI_all[, 1], 2, raw = TRUE))[[1]][1]
            Wald.CI_high <- lm(Wald.CI_all[, 3] ~ poly(Wald.CI_all[, 1], 2, raw = TRUE))[[1]][1]
          }
          else {
            Wald.CI_low <- lm(Wald.CI_all[, 2] ~ poly(Wald.CI_all[, 1], 3, raw = TRUE))[[1]][1]
            Wald.CI_high <- lm(Wald.CI_all[, 3] ~ poly(Wald.CI_all[, 1], 3, raw = TRUE))[[1]][1]
          }
        }
        list(t(sort(c(Wald.CI_low, Wald.CI_high))), warning_Wald)
      }
    }
    else {
      list(r, NULL)
    }
  }
  else {
    # Both Wald.CI and trans.Wald.CI need to be computed.
    r <- tryCatch(Wald_trans.Wald_nr(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv, max.mph.iter,
                                     step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                                     max.score.diff.iter, cut.off, S.space.H0, trans.g, trans.g.deriv, trans.g.inv),
                  error = function(e) {matrix(NA, nrow = 2, ncol = 2)})
    if (all(is.na(r))) {
      warning_this <- "Wald.CI and trans.Wald.CI: Adjustment used. Not on original data.\n"
      # robustifying procedure
      adj.epsilon.plus <- adj.epsilon * seq(1, iter.robust.max)
      Wald.CI_all <- matrix(NA, nrow = iter.robust.max, ncol = 2)
      trans.Wald.CI_all <- matrix(NA, nrow = iter.robust.max, ncol = 2)
      iter.robust <- 1
      iter.eff <- 0
      while (iter.robust <= iter.robust.max & iter.eff < iter.robust.eff) {
        r_temp <- tryCatch(Wald_trans.Wald_nr(y + adj.epsilon.plus[iter.robust], strata, fixed.strata, h0.fct, h0.fct.deriv,
                                              S0.fct, S0.fct.deriv, max.mph.iter, step, change.step.after, y.eps, iter.orig,
                                              norm.diff.conv, norm.score.conv, max.score.diff.iter, cut.off, S.space.H0,
                                              trans.g, trans.g.deriv, trans.g.inv),
                           error = function(e) {matrix(NA, nrow = 2, ncol = 2)})
        Wald.CI_all[iter.robust, ] <- r_temp[1, ]
        trans.Wald.CI_all[iter.robust, ] <- r_temp[2, ]
        iter.robust <- iter.robust + 1
        if (!any(is.na(r_temp))) {
          iter.eff <- iter.eff + 1
        }
      }
      Wald.CI_all <- cbind(adj.epsilon.plus, Wald.CI_all)
      Wald.CI_all <- as.matrix(na.omit(as.data.frame(Wald.CI_all)))
      trans.Wald.CI_all <- cbind(adj.epsilon.plus, trans.Wald.CI_all)
      trans.Wald.CI_all <- as.matrix(na.omit(as.data.frame(trans.Wald.CI_all)))
      if (nrow(Wald.CI_all) == 0) {
        list(matrix(NA, nrow = 2, ncol = 2), warning_this)
      }
      else {
        if (nrow(Wald.CI_all) <= 4) {
          Wald.CI_low <- lm(Wald.CI_all[, 2] ~ poly(Wald.CI_all[, 1], 1, raw = TRUE))[[1]][1]
          Wald.CI_high <- lm(Wald.CI_all[, 3] ~ poly(Wald.CI_all[, 1], 1, raw = TRUE))[[1]][1]
          trans.Wald.CI_low <- lm(trans.Wald.CI_all[, 2] ~ poly(trans.Wald.CI_all[, 1], 1, raw = TRUE))[[1]][1]
          trans.Wald.CI_high <- lm(trans.Wald.CI_all[, 3] ~ poly(trans.Wald.CI_all[, 1], 1, raw = TRUE))[[1]][1]
        }
        else {
          if (nrow(Wald.CI_all) <= 7) {
            Wald.CI_low <- lm(Wald.CI_all[, 2] ~ poly(Wald.CI_all[, 1], 2, raw = TRUE))[[1]][1]
            Wald.CI_high <- lm(Wald.CI_all[, 3] ~ poly(Wald.CI_all[, 1], 2, raw = TRUE))[[1]][1]
            trans.Wald.CI_low <- lm(trans.Wald.CI_all[, 2] ~ poly(trans.Wald.CI_all[, 1], 2, raw = TRUE))[[1]][1]
            trans.Wald.CI_high <- lm(trans.Wald.CI_all[, 3] ~ poly(trans.Wald.CI_all[, 1], 2, raw = TRUE))[[1]][1]
          }
          else {
            Wald.CI_low <- lm(Wald.CI_all[, 2] ~ poly(Wald.CI_all[, 1], 3, raw = TRUE))[[1]][1]
            Wald.CI_high <- lm(Wald.CI_all[, 3] ~ poly(Wald.CI_all[, 1], 3, raw = TRUE))[[1]][1]
            trans.Wald.CI_low <- lm(trans.Wald.CI_all[, 2] ~ poly(trans.Wald.CI_all[, 1], 3, raw = TRUE))[[1]][1]
            trans.Wald.CI_high <- lm(trans.Wald.CI_all[, 3] ~ poly(trans.Wald.CI_all[, 1], 3, raw = TRUE))[[1]][1]
          }
        }
        Wald.CI <- sort(c(Wald.CI_low, Wald.CI_high))
        trans.Wald.CI <- sort(c(trans.Wald.CI_low, trans.Wald.CI_high))
        list(t(cbind(Wald.CI, trans.Wald.CI)), warning_this)
      }
    }
    else {
      list(r, NULL)
    }
  }
}

