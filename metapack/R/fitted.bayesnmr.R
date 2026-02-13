#' get fitted values
#' 
#' @param object the output model from fitting a meta analysis/regression model
#' @param level credible level for interval estimation; set to 0.95 by default
#' @param HPD a logical argument indicating whether HPD intervals should be computed; if FALSE, equal-tail credible intervals are computed
#' @param ... additional arguments for fitted
#' @return a list of fitted values
#' @method fitted bayesnmr
#' @export
"fitted.bayesnmr" <- function(object, level = 0.95, HPD = TRUE, ...) {
	out <- list()
	theta <- list()
	phi <- list()
	gam <- list()
	sig2 <- list()
	Rho <- list()
	if (object$scale_x) {
		xcols <- ncol(object$XCovariate)
		tlength <- nrow(object$mcmc.draws$theta)
		trlength <- tlength - xcols
		tscale <- c(unname(attr(object$XCovariate, "scaled:scale")), rep(1, trlength))
	} else {
		tlength <- nrow(object$mcmc.draws$theta)
		tscale <- rep(1, tlength)
	}
	theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
		object$mcmc.draws$theta[,ikeep] / tscale
	}, FUN.VALUE = numeric(tlength))
	theta$mean <- rowMeans(theta.post)
	theta$sd <- apply(theta.post, 1, sd)
	phi$mean <- rowMeans(object$mcmc.draws$phi)
	phi$sd <- apply(object$mcmc.draws$phi, 1, sd)
	gam$mean <- rowMeans(object$mcmc.draws$gam)
	gam$sd <- apply(object$mcmc.draws$gam, 1, sd)
	sig2$mean <- rowMeans(object$mcmc.draws$sig2)
	sig2$sd <- apply(object$mcmc.draws$sig2, 1, sd)
	Rho$mean <- apply(object$mcmc.draws$Rho, c(1,2), mean)
	Rho$sd <- apply(object$mcmc.draws$Rho, c(1,2), sd)
	if (object$control$sample_df) {
		df <- list()
		df$mean <- mean(object$mcmc.draws$df)
		df$sd <- sd(object$mcmc.draws$df)
	}

	sig.level <- 1 - level

	if (HPD) {
		theta.hpd <- mhpd(theta.post, level)
		theta$lower <- theta.hpd[,1]
		theta$upper <- theta.hpd[,2]

		phi.hpd <- mhpd(object$mcmc.draws$phi, level)
		phi$lower <- phi.hpd[,1]
		phi$upper <- phi.hpd[,2]

		gam.hpd <- mhpd(object$mcmc.draws$gam, level)
		gam$lower <- gam.hpd[,1]
		gam$upper <- gam.hpd[,2]

		sig2.hpd <- mhpd(object$mcmc.draws$sig2, level)
		sig2$lower <- sig2.hpd[,1]
		sig2$upper <- sig2.hpd[,2]

		Rho.hpd <- hpdarray(object$mcmc.draws$Rho, level = level)
		Rho$lower <- Rho.hpd[,,1]
		Rho$upper <- Rho.hpd[,,2]

		if (object$control$sample_df) {
			df.hpd <- vhpd(object$mcmc.draws$df, level)
			df$lower <- df.hpd[,1]
			df$upper <- df.hpd[,2]			
		}
    } else {
        theta$lower <- apply(theta.post, 1, function(xx) quantile(xx, prob = sig.level/2))
		theta$upper <- apply(theta.post, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

		phi$lower <- apply(object$mcmc.draws$phi, 1, function(xx) quantile(xx, prob = sig.level/2))
		phi$upper <- apply(object$mcmc.draws$phi, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

		gam$lower <- apply(object$mcmc.draws$gam, 1, function(xx) quantile(xx, prob = sig.level/2))
		gam$upper <- apply(object$mcmc.draws$gam, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

		sig2$lower <- apply(object$mcmc.draws$sig2, 1, function(xx) quantile(xx, prob = sig.level/2))
		sig2$upper <- apply(object$mcmc.draws$sig2, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

		Rho.ci <- ciarray(object$mcmc.draws$Rho, level = level)
		Rho$lower <- Rho.ci[,,1]
		Rho$upper <- Rho.ci[,,2]

		if (object$control$sample_df) {
			df$lower <- quantile(object$mcmc.draws$df, prob = sig.level/2)
			df$upper <- quantile(object$mcmc.draws$df, prob = 1-sig.level/2)
		}
	}

	out <- list()
	out$level <- level
	out$hpd <- HPD
	out$theta <- theta
	out$phi <- phi
	out$gam <- gam
	out$sig2 <- sig2
	out$Rho <- Rho
	if (object$control$sample_df) {
		out$df <- df
	}
	class(out) <- "fitted.bayesnmr"
	out
}
