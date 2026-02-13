#' get fitted values
#' 
#' @param object the output model from fitting a meta analysis/regression model
#' @param level credible level for interval estimation; set to 0.95 by default
#' @param HPD a logical argument indicating whether HPD intervals should be computed; if FALSE, equal-tail credible intervals are computed
#' @param ... additional arguments for fitted
#' @return a list of fitted values
#' @method fitted bayesparobs
#' @export
"fitted.bayesparobs" <- function(object, level = 0.95, HPD = TRUE, ...) {
	out <- list()
	fmodel <- object$fmodel
	ypred <- list()
	ypred$mean <- apply(object$mcmc.draws$ypred, c(1,2), mean)
	ypred$sd <- apply(object$mcmc.draws$ypred, c(1,2), sd)

	if (object$scale_x) {
		J <- ncol(object$Outcome)
		xcols <- ncol(object$XCovariate)
		tlength <- nrow(object$mcmc.draws$theta)
		trlength <- tlength - xcols * J
		tscale <- c(rep(unname(attr(object$XCovariate, "scaled:scale")), J), rep(1, trlength))
	} else {
		tlength <- nrow(object$mcmc.draws$theta)
		tscale <- rep(1, tlength)
	}
	if (object$fmodel == 1) {
		theta <- list()
		theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
			object$mcmc.draws$theta[,ikeep] / tscale
		}, FUN.VALUE = numeric(tlength))
		theta$mean <- rowMeans(theta.post)
		theta$sd <- apply(theta.post, 1, sd)
		Omega <- list()
		Omega$mean <- apply(object$mcmc.draws$Omega, c(1,2), mean)
		Omega$sd <- apply(object$mcmc.draws$Omega, c(1,2), sd)
		Sigma <- list()
		Sigma$mean <- apply(object$mcmc.draws$Sigma, c(1,2), mean)
		Sigma$sd <- apply(object$mcmc.draws$Sigma, c(1,2), sd)
	} else if (object$fmodel == 2) {
		theta <- list()
		theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
			object$mcmc.draws$theta[,ikeep] / tscale
		}, FUN.VALUE = numeric(tlength))
		theta$mean <- rowMeans(theta.post)
		theta$sd <- apply(theta.post, 1, sd)
		Omega <- list()
		Omega$mean <- apply(object$mcmc.draws$Omega, c(1,2), mean)
		Omega$sd <- apply(object$mcmc.draws$Omega, c(1,2), sd)
		Sigma <- list()
		Sigma$mean <- apply(object$mcmc.draws$Sigma, c(1,2), mean)
		Sigma$sd <- apply(object$mcmc.draws$Sigma, c(1,2), sd)
		R <- list()
		R$mean <- apply(object$mcmc.draws$R, c(1,2), mean)
		R$sd <- apply(object$mcmc.draws$R, c(1,2), sd)
	} else if (object$fmodel == 3) {
		theta <- list()
		theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
			object$mcmc.draws$theta[,ikeep] / tscale
		}, FUN.VALUE = numeric(tlength))
		theta$mean <- rowMeans(theta.post)
		theta$sd <- apply(theta.post, 1, sd)
		Omega <- list()
		Omega$mean <- apply(object$mcmc.draws$Omega, c(1,2), mean)
		Omega$sd <- apply(object$mcmc.draws$Omega, c(1,2), sd)
		Sigma <- list()
		Sigma$mean <- apply(object$mcmc.draws$Sigma, c(1,2), mean)
		Sigma$sd <- apply(object$mcmc.draws$Sigma, c(1,2), sd)
		R <- list()
		R$mean <- apply(object$mcmc.draws$R, c(1,2), mean)
		R$sd <- apply(object$mcmc.draws$R, c(1,2), sd)
	} else if (object$fmodel == 4) {
		theta <- list()
		theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
			object$mcmc.draws$theta[,ikeep] / tscale
		}, FUN.VALUE = numeric(tlength))
		theta$mean <- rowMeans(theta.post)
		theta$sd <- apply(theta.post, 1, sd)
		Omega <- list()
		Omega$mean <- apply(object$mcmc.draws$Omega, c(1,2), mean)
		Omega$sd <- apply(object$mcmc.draws$Omega, c(1,2), sd)
		Sigma <- list()
		Sigma$mean <- apply(object$mcmc.draws$Sigma, c(1,2), mean)
		Sigma$sd <- apply(object$mcmc.draws$Sigma, c(1,2), sd)
		delta <- list()
		delta$mean <- apply(object$mcmc.draws$delta, c(1,2), mean)
		delta$sd <- apply(object$mcmc.draws$delta, c(1,2), sd)
		Rho <- list()
		Rho$mean <- apply(object$mcmc.draws$Rho, c(1,2), mean)
		Rho$sd <- apply(object$mcmc.draws$Rho, c(1,2), sd)
		R <- list()
		R$mean <- apply(object$mcmc.draws$R, c(1,2), mean)
		R$sd <- apply(object$mcmc.draws$R, c(1,2), sd)
	} else if (object$fmodel == 5) {
		theta <- list()
		theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
			object$mcmc.draws$theta[,ikeep] / tscale
		}, FUN.VALUE = numeric(tlength))
		theta$mean <- rowMeans(theta.post)
		theta$sd <- apply(theta.post, 1, sd)
		Omega <- list()
		Omega$mean <- apply(object$mcmc.draws$Omega, c(1,2), mean)
		Omega$sd <- apply(object$mcmc.draws$Omega, c(1,2), sd)
		Sigma <- list()
		Sigma$mean <- apply(object$mcmc.draws$Sigma, c(1,2), mean)
		Sigma$sd <- apply(object$mcmc.draws$Sigma, c(1,2), sd)
		Delta <- list()
		Delta$mean <- rowMeans(object$mcmc.draws$delta)
		Delta$sd <- apply(object$mcmc.draws$delta, 1, sd)
		Rho <- list()
		Rho$mean <- apply(object$mcmc.draws$Rho, c(1,2), mean)
		Rho$sd <- apply(object$mcmc.draws$Rho, c(1,2), sd)
		Sigma0 <- list()
		Sigma0$mean <- apply(object$mcmc.draws$Sigma0, c(1,2), mean)
		Sigma0$sd <- apply(object$mcmc.draws$Sigma0, c(1,2), sd)
		R <- list()
		R$mean <- apply(object$mcmc.draws$R, c(1,2), mean)
		R$sd <- apply(object$mcmc.draws$R, c(1,2), sd)
	}
	sig.level <- 1 - level

	if (HPD) {
		theta.hpd <- mhpd(theta.post, level)
		theta$lower <- theta.hpd[,1]
		theta$upper <- theta.hpd[,2]

		Omega.hpd <- hpdarray(object$mcmc.draws$Omega, level = level)
		Omega$lower <- Omega.hpd[,,1]
		Omega$upper <- Omega.hpd[,,2]
		Sigma.hpd <- hpdarray(object$mcmc.draws$Sigma, level = level)
		Sigma$lower <- Sigma.hpd[,,1]
		Sigma$upper <- Sigma.hpd[,,2]

		if (fmodel >= 2) {
			R.hpd <- hpdarray(object$mcmc.draws$R, level = level)
			R$lower <- R.hpd[,,1]
			R$upper <- R.hpd[,,2]

			if (fmodel == 4) {
				delta.hpd <- hpdarray(object$mcmc.draws$delta, level = level)
				delta$lower <- delta.hpd[,,1]
				delta$upper <- delta.hpd[,,2]

				Rho.hpd <- hpdarray(object$mcmc.draws$Rho, level = level)
				Rho$lower <- Rho.hpd[,,1]
				Rho$upper <- Rho.hpd[,,2]
			} else if (fmodel == 5) {
				Delta.hpd <- mhpd(object$mcmc.draws$delta, level)
				Delta$lower <- Delta.hpd[,1]
				Delta$upper <- Delta.hpd[,2]

				Rho.hpd <- hpdarray(object$mcmc.draws$Rho, level = level)
				Rho$lower <- Rho.hpd[,,1]
				Rho$upper <- Rho.hpd[,,2]
				
				Sigma0.hpd <- hpdarray(object$mcmc.draws$Sigma0, level = level)
				Sigma0$lower <- Sigma0.hpd[,,1]
				Sigma0$upper <- Sigma0.hpd[,,2]
			}
		}
	} else {
		theta$lower <- apply(theta.post, 1, function(xx) quantile(xx, prob = sig.level/2))
		theta$upper <- apply(theta.post, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

		Omega.ci <- ciarray(object$mcmc.draws$Omega, level = level)
		Omega$lower <- Omega.ci[,,1]
		Omega$upper <- Omega.ci[,,2]
		Sigma.ci <- ciarray(object$mcmc.draws$Sigma, level = level)
		Sigma$lower <- Sigma.ci[,,1]
		Sigma$upper <- Sigma.ci[,,2]

		if (fmodel >= 2) {
			R.ci <- ciarray(object$mcmc.draws$R, level = level)
			R$lower <- R.ci[,,1]
			R$upper <- R.ci[,,2]
			if (fmodel == 4) {
				delta.ci <- ciarray(object$mcmc.draws$delta, level = level)
				delta$lower <- delta.ci[,,1]
				delta$upper <- delta.ci[,,2]

				Rho.ci <- ciarray(object$mcmc.draws$Rho, level = level)
				Rho$lower <- Rho.ci[,,1]
				Rho$upper <- Rho.ci[,,2]
			} else if (fmodel == 5) {
				Delta$lower <- apply(object$mcmc.draws$delta, 1, function(xx) quantile(xx, prob = sig.level/2))
				Delta$upper <- apply(object$mcmc.draws$delta, 1, function(xx) quantile(xx, prob = 1-sig.level/2))

				Rho.ci <- ciarray(object$mcmc.draws$Rho, level = level)
				Rho$lower <- Rho.ci[,,1]
				Rho$upper <- Rho.ci[,,2]
				
				Sigma0.ci <- ciarray(object$mcmc.draws$Sigma0, level = level)
				Sigma0$lower <- Sigma0.ci[,,1]
				Sigma0$upper <- Sigma0.ci[,,2]
			}
		}
	}

	out <- list()
	out$level <- level
	out$hpd <- HPD
	out$ypred <- ypred
	out$theta <- theta
	out$Sigma <- Sigma
	out$Omega <- Omega
	if (fmodel >= 2) {
		out$R <- R
		if (fmodel == 4) {
			out$delta <- delta
			out$Rho <- Rho
		} else if (fmodel == 5) {
			out$Delta <- Delta
			out$Rho <- Rho
			out$Sigma0 <- Sigma0
		}
	}

	class(out) <- "fitted.bayesparobs"
	return(out)
}
