#' Print results
#' 
#' @param x the output model from fitting a meta analysis/regression model
#' @param level credible level for interval estimation; set to 0.95 by default
#' @param HPD a logical argument indicating whether HPD intervals should be computed; if FALSE, equal-tail credible intervals are computed
#' @param ... additional arguments for print
#' @return No return value; print a summary of the output
#' @importFrom stats sd quantile
#' @export
"print.bayesparobs" <- function(x, level=0.95, HPD=TRUE, ...) {
	if (inherits(x, "bsynthesis")) {
		cat("\nCall:\n", paste(deparse(x$call), sep = "\n", 
	        collapse = "\n"), sep = "")
	} else {
		cat("Bayesian Inference for Multivariate Meta-Regression\nWith a Partially Observed Within-Study Sample Covariance Matrix\n")
	}
	cat("\n")
	cat("Model:\n")
	cat("  (Aggregate mean)\n    y_kt = X_kt * theta + W_kt * gamma_k + N(0, Sigma_kt / n_kt)\n")
	cat("  (Sample Variance)\n    (n_kt - 1) S_kt ~ Wishart(n_kt - 1, Sigma_kt)\n")
	cat("  (Random effects)\n    ")
	cat("[gamma_k | Omega] ~ N(0, Omega)\n")
	digits <- max(3, getOption("digits") - 3)
	cat("Priors:\n")
	cat("   theta ~ MVN(0, ",x$prior$c0," * I_p)\n")
	cat("   Omega_j^{-1} ~ Wishart(",x$prior$dj0,", Omega0)\n")
	if (x$fmodel == 1) {
		cat("   Sigma_kt = diag(sig_{tk,11}^2, ..., sig_{tk,JJ}^2)\n")
		cat("   where sig_{tk,jj}^2 ~ IG(",x$prior$s0,", ",x$prior$d0,")\n")
	} else if (x$fmodel == 2) {
		cat("   Sigma_kt = Sigma, where Sigma ~ Wishart(",x$prior$s0,", Sigma0)\n")
	} else if (x$fmodel == 3) {
		cat("   Sigma_kt = Sigma_t, where Sigma_t ~ Wishart(",x$prior$s0,", Sigma0)\n")
	} else if (x$fmodel == 4) {
		cat("   Sigma_{tk} = sig_{tk} * Rho * sig_{tk},\n")
		cat("   	where p(Rho) = 1, and sig_{tk,jj} ~ IG(",x$prior$s0,", ",x$prior$d0,")\n")
	} else if (x$fmodel == 5) {
		cat("   Sigma_{tk}^{-1} ~ Wishart(",x$prior$nu0," (nu0-J-1)*Sigma),\n")
		cat("   Sigma ~ Wishart(",x$prior$d0,", Sigma0),\n")
	}
	cat("---------------------------------------------------\n")
	cat("Number of trials:     ", x$K, "\n")
	cat("Number of arms:       ", nrow(x$Outcome), "\n")
	cat("Number of treatments: ", x$T, "\n")
	
	if (x$scale_x) {
		J <- ncol(x$Outcome)
		xcols <- ncol(x$XCovariate)
		tlength <- nrow(x$mcmc.draws$theta)
		trlength <- tlength - xcols * J
		tscale <- c(rep(unname(attr(x$XCovariate, "scaled:scale")), J), rep(1, trlength))
	} else {
		tlength <- nrow(x$mcmc.draws$theta)
		tscale <- rep(1, tlength)
	}
	theta <- list()
	theta.post <- vapply(1:x$mcmc$nkeep, function(ikeep) {
		x$mcmc.draws$theta[,ikeep] / tscale
	}, FUN.VALUE = numeric(tlength))
	theta <- list()
	theta$mean <- rowMeans(theta.post)
	theta$sd <- apply(theta.post, 1, sd)

	sig.level <- 1 - level

	if (HPD) {
		theta.hpd <- mhpd(theta.post, level)
		theta$lower <- theta.hpd[,1]
		theta$upper <- theta.hpd[,2]
	} else {
		theta$lower <- apply(theta.post, 1, function(xx) quantile(xx, prob = sig.level/2))
		theta$upper <- apply(theta.post, 1, function(xx) quantile(xx, prob = 1-sig.level/2))
	}
	theta_print <- cbind(theta$mean, theta$sd, theta$lower, theta$upper)
	if (HPD) {
		colnames(theta_print) <- c("Post.Mean", "Std.Dev", "HPD(Lower)", "HPD(Upper)")
	} else {
		colnames(theta_print) <- c("Post.Mean", "Std.Dev", "CI(Lower)", "CI(Upper)")
	}
	xcc <- if (!is.null(colnames(x$XCovariate))) colnames(x$XCovariate) else paste0("beta", 1:ncol(x$XCovariate))
	wcc <- if (!is.null(colnames(x$WCovariate))) colnames(x$WCovariate) else paste0("gam", 1:ncol(x$WCovariate))

	J <- ncol(x$Outcome)
	if (is.null(x$group)) {
		rownames(theta_print) <- c(paste0(rep(xcc, J), "_", rep(1:J, each=length(xcc))), paste0(rep(wcc, J), "_", rep(1:J, each=length(wcc))))
	} else {
		rownames(theta_print) <- c(paste0(rep(xcc, J), "_", rep(1:J, each=length(xcc))),
						 paste0(rep(wcc, 2*J), rep(rep(c("*(1-2nd)", "*2nd"), each = length(wcc)), J), "_", rep(1:J, each = 2*length(wcc))))
	}
	theta_print <- round(theta_print, digits=digits)
	print.default(theta_print, print.gap = 2)
	cat("---------------------------------------------------\n")
	if (HPD) {
		cat("*HPD level: ", level, "\n")
	} else {
		cat("*Credible level: ", level, "\n")
	}
	invisible()
}
