#' `summary` method for class "`bayesnmr`"
#' 
#' @param object the output model from fitting a network meta analysis/regression model
#' @param level credible level for interval estimation; set to 0.95 by default
#' @param HPD a logical argument indicating whether HPD intervals should be computed; if FALSE, equal-tail credible intervals are computed
#' @param ... additional arguments for print
#' @return does not return anything; print a summary of the output
#' @importFrom stats quantile sd
#' @method summary bayesnmr
#' @export
"summary.bayesnmr" <- function(object, level=0.95, HPD=TRUE, ...) {
	digits <- max(3, getOption("digits") - 3)
	theta <- list()
	phi <- list()
	gam <- list()
	sig2 <- list()
	Rho <- list()
	
	param <- object$mcmc.draws$theta
	if (object$scale_x) {
		xcols <- ncol(object$XCovariate)
		tlength <- nrow(param)
		trlength <- tlength - xcols
		tscale <- c(unname(attr(object$XCovariate, "scaled:scale")), rep(1, trlength))
	} else {
		tlength <- nrow(param)
		tscale <- rep(1, tlength)
	}
	theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
		param[,ikeep] / tscale
	}, FUN.VALUE = numeric(tlength))
	theta$mean <- rowMeans(theta.post)
	theta$sd <- apply(theta.post, 1, sd)
	phi$mean <- rowMeans(object$mcmc.draws$phi)
	phi$sd <- apply(object$mcmc.draws$phi, 1, sd)
	gam$mean <- rowMeans(object$mcmc.draws$gam)
	gam$sd <- apply(object$mcmc.draws$gam, 1, sd)

	level <- 0.95
	sig.level <- 1 - level

	if (HPD) {
		theta.hpd <- mhpd(theta.post, level)
		theta$lower <- theta.hpd[,1]
		theta$upper <- theta.hpd[,2]
	} else {
		theta$lower <- apply(theta.post, 1, function(xx) quantile(xx, prob = sig.level/2))
		theta$upper <- apply(theta.post, 1, function(xx) quantile(xx, prob = 1-sig.level/2))
	}
	if (inherits(object, "bsynthesis")) {
		cat("\nCall:\n", paste(deparse(object$call), sep = "\n", 
	        collapse = "\n"), "\n", sep = "")
	}
	r <- cbind(theta$mean, theta$sd, theta$lower, theta$upper)
	if (HPD) {
		colnames(r) <- c("Post.Mean", "Std.Dev", "HPD(Lower)", "HPD(Upper)")
	} else {
		colnames(r) <- c("Post.Mean", "Std.Dev", "CI(Lower)", "CI(Upper)")
	}
	cat("\nPosterior inference in network meta-regression models\n")
	cat("Fixed-effects:\n")
	r <- round(r, digits=digits)
	print.default(r, print.gap = 2)
	cat("---------------------------------------------------\n")
	if (HPD) {
		cat("*HPD level: ", level, "\n")
	} else {
		cat("*Credible level: ", level, "\n")
	}
	invisible(r)
}
