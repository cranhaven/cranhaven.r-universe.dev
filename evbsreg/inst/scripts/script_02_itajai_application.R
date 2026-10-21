################################################################
## script_02_itajai_application.R
##
## Reproduces the Itajai wind gust application of the paper:
##   - Table 1: descriptive statistics
##   - Table 2: full-sample EVBS regression fit
##   - Figure 3: scatter (wind vs pressure) + ACF
##   - Figure 4: randomized quantile residuals with envelope
##   - Figure 5: normalized eigenvalues + aggregate contributions B_j(7)
##   - Table 3: deletion analysis (drop obs #82 and #108)
##   - Figure 6: fitted regression curve over data
##
## Usage (after installing the package):
##   library(evbsreg)
##   source(system.file("scripts/script_02_itajai_application.R",
##                       package = "evbsreg"))
################################################################

library(evbsreg)
data(itajai)

resposta <- itajai$wind
x3       <- itajai$pressure
UMX3     <- cbind(1, x3)

# -----------------------------------------------------------------
# Table 1: Descriptive statistics
# -----------------------------------------------------------------
cat("===========================================================\n")
cat("Table 1: Descriptive statistics for the wind gust series\n")
cat("===========================================================\n")
desc <- c(
  n        = length(resposta),
  Minimum  = min(resposta),
  Median   = median(resposta),
  Mean     = mean(resposta),
  Maximum  = max(resposta),
  SD       = sd(resposta),
  Skewness = sum((resposta - mean(resposta))^3) /
               (length(resposta) * sd(resposta)^3),
  Kurtosis = sum((resposta - mean(resposta))^4) /
               (length(resposta) * sd(resposta)^4) - 3
)
print(round(desc, 3))

# -----------------------------------------------------------------
# Figure 3: scatter + ACF
# -----------------------------------------------------------------
cat("\nFigure 3: producing scatter + ACF...\n")
png(file.path(tempdir(), "fig3_scatter_acf.png"), width = 1500, height = 700, res = 150)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.5, 1.2))
plot(x3, resposta,
     ylim = c(0, 35), xlim = c(1000, 1035),
     xlab = "Average atmospheric pressure (mB)",
     ylab = "Monthly maximum wind speed (m/s)",
     pch = 16, cex = 0.9, col = "darkgreen", main = "(a)")
abline(lm(resposta ~ x3), col = "red", lwd = 1.5)
acf(resposta, main = "(b)", ylab = "Autocorrelation function", xlab = "Lag")
dev.off()

# -----------------------------------------------------------------
# Table 2: Full-sample EVBS regression fit
# -----------------------------------------------------------------
cat("\n===========================================================\n")
cat("Table 2: Full-sample EVBS regression fit (n=124)\n")
cat("===========================================================\n")
fit <- evbsreg.fit(UMX3, resposta)
tab2 <- data.frame(
  Parameter = c("beta0 (Intercept)", "beta1 (pressure)", "alpha", "gamma"),
  MLE       = round(fit$coeff, 4),
  SE        = round(c(fit$stderrors, fit$stderroralpha, fit$stderrorgama), 4),
  z         = round(c(fit$zstats, NA, NA), 4),
  p.value   = round(c(fit$pvalues, NA, NA), 4)
)
print(tab2)

# -----------------------------------------------------------------
# Figure 4: residual envelope
# -----------------------------------------------------------------
cat("\nFigure 4: producing residual envelope...\n")
png(file.path(tempdir(), "fig4_envelope.png"), width = 800, height = 800, res = 150)
envelope_qq(UMX3, resposta, nrep = 100)
dev.off()

rqr <- rqrandomized(UMX3, resposta)
cat("\nGoodness of fit of randomized residuals:\n")
cat(sprintf("  Shapiro-Wilk p-value:       %.4f\n",
            shapiro.test(rqr)$p.value))
cat(sprintf("  Kolmogorov-Smirnov p-value: %.4f\n",
            ks.test(rqr, "pnorm")$p.value))

# -----------------------------------------------------------------
# Figure 5: CNC diagnostics
# -----------------------------------------------------------------
cat("\nFigure 5: producing CNC diagnostic panels...\n")
diag_out <- cnc_diagnostics(fit)
png(file.path(tempdir(), "fig5_cnc.png"), width = 1500, height = 700, res = 150)
plot_cnc(diag_out, q = 7, label.flagged = 5)
dev.off()

cat("\nTop 4 normalized eigenvalues (|lambda*|):\n")
cat(sprintf("  %.5f  %.5f  %.5f  %.5f\n",
            diag_out$eigenvalues_norm[1], diag_out$eigenvalues_norm[2],
            diag_out$eigenvalues_norm[3], diag_out$eigenvalues_norm[4]))
cat(sprintf("Reference value b(q=7): %.4f\n", diag_out$bq[7]))

flagged <- which(diag_out$Bj[7, ] > diag_out$bq[7])
cat(sprintf("Observations flagged by B_j(7): %s\n",
            paste(flagged, collapse = ", ")))

# -----------------------------------------------------------------
# Table 3: Deletion analysis
# -----------------------------------------------------------------
cat("\n===========================================================\n")
cat("Table 3: Sensitivity of estimates to removal of flagged obs\n")
cat("===========================================================\n")
for (obs in c(82, 108)) {
  UMX_del  <- UMX3[-obs, ]
  resp_del <- resposta[-obs]
  fit_del  <- evbsreg.fit(UMX_del, resp_del)
  Rc <- 100 * (fit_del$coeff - fit$coeff) / abs(fit$coeff)
  cat(sprintf("\nDrop obs #%d (value %.1f m/s):\n", obs, resposta[obs]))
  cat(sprintf("  beta0 = %8.4f  (delta = %+.2f%%)\n", fit_del$coeff[1], Rc[1]))
  cat(sprintf("  beta1 = %8.4f  (delta = %+.2f%%)\n", fit_del$coeff[2], Rc[2]))
  cat(sprintf("  alpha = %8.4f  (delta = %+.2f%%)\n", fit_del$coeff[3], Rc[3]))
  cat(sprintf("  gamma = %8.4f  (delta = %+.2f%%)\n", fit_del$coeff[4], Rc[4]))
}

# -----------------------------------------------------------------
# Figure 6: Fitted regression curve
# -----------------------------------------------------------------
cat("\nFigure 6: producing fitted regression curve...\n")
png(file.path(tempdir(), "fig6_fitted_curve.png"), width = 900, height = 700, res = 150)
par(mar = c(4.5, 4.5, 2, 1))
ord <- order(x3)
fitted_median <- exp(fit$coeff[1] + fit$coeff[2] * x3[ord])
plot(x3, resposta, ylim = c(0, 35), xlim = c(1000, 1035),
     xlab = "Average atmospheric pressure (mB)",
     ylab = "Monthly maximum wind speed (m/s)",
     pch = 16, cex = 0.9, col = "darkgreen")
lines(x3[ord], fitted_median, col = "red", lwd = 2)
legend("topright", legend = c("Observed", "Fitted EVBS median"),
       pch = c(16, NA), lty = c(NA, 1),
       col = c("darkgreen", "red"), bty = "n")
dev.off()

cat("\n===========================================================\n")
cat("Itajai application complete.\n")
cat("===========================================================\n")
