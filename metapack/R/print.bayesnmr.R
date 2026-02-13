#' Print results
#' 
#' @param x the output model from fitting a network meta analysis/regression model
#' @param level credible level for interval estimation; set to 0.95 by default
#' @param HPD a logical argument indicating whether HPD intervals should be computed; if FALSE, equal-tail credible intervals are computed
#' @param ... additional arguments for print
#' @return No return value; print a summary of the output
#' @importFrom stats quantile sd
#' @export
"print.bayesnmr" <- function(x, level=0.95, HPD=TRUE, ...) {
	if (inherits(x, "bsynthesis")) {
		cat("\nCall:\n", paste(deparse(x$call), sep = "\n", 
	        collapse = "\n"), sep = "")
	} else {
		cat("Bayesian Network Meta-Regression Hierarchical Models\nUsing Heavy-Tailed Multivariate Random Effects\nwith Covariate-Dependent Variances\n")
	}
	cat("\n")
	cat("Model:\n")
	cat("  (Aggregate mean)\n    y_kt = x_kt'theta + tau_kt * gamma_kt + N(0, sigma_kt^2 / n_kt)\n")
	cat("  (Sample Variance)\n    (n_kt - 1) S^2 / sigma_kt^2 ~ chi^2(n_kt - 1)\n")
	cat("  (Random effects)\n    ")
	cat("[gam | Rho,nu] ~ MVT(0, E_k' Rho E_k, nu)\n")
	cat("Priors:\n")
	cat("  theta      ~ MVN(0, c01 * I_p), c01=", x$prior$c01, "\n")
	cat("  phi        ~ MVN(0, c02 * I_q), c02=", x$prior$c02, "\n")
	cat("  p(sigma^2) ~ 1/sigma^2 * I(sigma^2 > 0)\n")
	cat("  p(Rho)     ~ 1\n")
	cat("---------------------------------------------------\n")
	cat("Number of studies:    ", x$K, "\n")
	cat("Number of arms:       ", length(x$Outcome), "\n")
	cat("Number of treatments: ", x$nT, "\n")
	digits <- max(3, getOption("digits") - 3)
	theta <- list()
	phi <- list()
	gam <- list()
	sig2 <- list()
	Rho <- list()
	param <- x$mcmc.draws$theta
	if (x$scale_x) {
		xcols <- ncol(x$XCovariate)
		tlength <- nrow(param)
		trlength <- tlength - xcols
		tscale <- c(unname(attr(x$XCovariate, "scaled:scale")), rep(1, trlength))
	} else {
		tlength <- nrow(param)
		tscale <- rep(1, tlength)
	}
	theta.post <- vapply(1:x$mcmc$nkeep, function(ikeep) {
		param[,ikeep] / tscale
	}, FUN.VALUE = numeric(tlength))
	theta$mean <- rowMeans(theta.post)
	theta$sd <- apply(theta.post, 1, sd)
	phi$mean <- rowMeans(x$mcmc.draws$phi)
	phi$sd <- apply(x$mcmc.draws$phi, 1, sd)
	gam$mean <- rowMeans(x$mcmc.draws$gam)
	gam$sd <- apply(x$mcmc.draws$gam, 1, sd)

	sig.level <- 1 - level

	if (HPD) {
		theta.hpd <- mhpd(theta.post, level)
		theta$lower <- theta.hpd[,1]
		theta$upper <- theta.hpd[,2]

		phi.hpd <- mhpd(x$mcmc.draws$phi, level)
		phi$lower <- phi.hpd[,1]
		phi$upper <- phi.hpd[,2]
	} else {
		theta$lower <- apply(theta.post, 1, function(xx) quantile(xx, prob = sig.level/2))
		theta$upper <- apply(theta.post, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

		phi$lower <- apply(x$mcmc.draws$phi, 1, function(xx) quantile(xx, prob = sig.level/2))
		phi$upper <- apply(x$mcmc.draws$phi, 1, function(xx) quantile(xx, prob = 1-sig.level/2))
	}
	theta_print <- cbind(theta$mean, theta$sd, theta$lower, theta$upper)
	phi_print <- cbind(phi$mean, phi$sd, phi$lower, phi$upper)
	p_print <- rbind(theta_print, phi_print)
	if (HPD) {
		colnames(p_print) <- c("Post.Mean", "Std.Dev", "HPD(Lower)", "HPD(Upper)")
	} else {
		colnames(p_print) <- c("Post.Mean", "Std.Dev", "CI(Lower)", "CI(Upper)")
	}
	rownames(p_print) <- c(rownames(theta_print),
						   paste0("phi", 1:length(phi$mean)))
	p_print <- round(p_print, digits=digits)
	print.default(p_print, print.gap = 2)
	cat("---------------------------------------------------\n")
	if (HPD) {
		cat("*HPD level: ", level, "\n")
	} else {
		cat("*Credible level: ", level, "\n")
	}
	invisible()
}
