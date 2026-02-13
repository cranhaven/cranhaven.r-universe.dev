#' get the highest posterior density (HPD) interval or equal-tailed credible interval
#' @param object the output model from fitting a (network) meta analysis/regression model
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the probability which the HPD interval will cover
#' @param HPD a logical value indicating whether HPD or equal-tailed credible interval should be computed; by default, TRUE
#' @return dataframe containing HPD intervals for the parameters
#' @method hpd bayesparobs
#' @export
"hpd.bayesparobs" <- function(object, parm, level = 0.95, HPD = TRUE) {
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
	theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
		object$mcmc.draws$theta[,ikeep] / tscale
	}, FUN.VALUE = numeric(tlength))
	if (missing(parm)) {
		out <- list()
		if (HPD) {
			out$ypred <- hpdarray(object$mcmc.draws$ypred, level = level)
			out$theta <- mhpd(theta.post, level)
			out$Omega <- hpdarray(object$mcmc.draws$Omega, level = level)
			out$Sigma <- hpdarray(object$mcmc.draws$Sigma, level = level)
			if (object$fmodel >= 2) {
				out$R <- hpdarray(object$mcmc.draws$R, level = level)
				if (object$fmodel == 4) {
					out$delta <- hpdarray(object$mcmc.draws$delta, level = level)
					out$Rho <- hpdarray(object$mcmc.draws$Rho, level = level)
				} else if (object$fmodel == 5) {
					out$delta <- mhpd(object$mcmc.draws$delta, level)
					out$Rho <- hpdarray(object$mcmc.draws$Rho, level = level)
					out$Sigma0 <- hpdarray(object$mcmc.draws$Sigma0, level = level)
				}
			}
		} else {
			sig.level <- 1 - level
			out$ypred <- ciarray(object$mcmc.draws$ypred, level = level)
			out$theta <- apply(theta.post, 1, function(xx) quantile(xx, c(sig.level/2, 1 - sig.level/2)))
			out$Omega <- ciarray(object$mcmc.draws$Omega, level = level)
			out$Sigma <- ciarray(object$mcmc.draws$Sigma, level = level)
			if (object$fmodel >= 2) {
				out$R <- ciarray(object$mcmc.draws$R, level = level)
				if (object$fmodel == 4) {
					out$delta <- ciarray(object$mcmc.draws$delta, level = level)
					out$Rho <- ciarray(object$mcmc.draws$Rho, level = level)
				} else if (object$fmodel == 5) {
					out$delta <- apply(object$mcmc.draws$delta, 1, function(xx) quantile(xx, c(sig.level/2, 1 - sig.level/2)))
					out$Rho <- ciarray(object$mcmc.draws$Rho, level = level)
					out$Sigma0 <- ciarray(object$mcmc.draws$Sigma0, level = level)
				}
			}
		}
		class(out) <- "bayesparobs.hpd"
		return(out)
	} else {
		cl <- list()
		if (HPD) {
			cl$ypred <- quote(hpdarray(object$mcmc.draws$ypred, level=level))
			cl$theta <- quote(mhpd(theta.post, level))
			cl$Omega <- quote(hpdarray(object$mcmc.draws$Omega, level = level))
			cl$Sigma <- quote(hpdarray(object$mcmc.draws$Sigma, level = level))
			if (object$fmodel >= 2) {
				cl$R <- quote(hpdarray(object$mcmc.draws$R, level = level))
				if (object$fmodel == 4) {
					cl$delta <- quote(hpdarray(object$mcmc.draws$delta, level = level))
					cl$Rho <- quote(hpdarray(object$mcmc.draws$Rho, level = level))
				} else if (object$fmodel == 5) {
					cl$delta <- quote(mhpd(object$mcmc.draws$delta, level))
					cl$Rho <- quote(hpdarray(object$mcmc.draws$Rho, level = level))
					cl$Sigma0 <- quote(hpdarray(object$mcmc.draws$Sigma0, level = level))
				}
			}
		} else {
			sig.level <- 1 - level
			cl$ypred <- quote(ciarray(object$mcmc.draws$ypred, level = level))
			cl$theta <- quote(apply(theta.post, 1, function(xx) quantile(xx, c(sig.level/2, 1 - sig.level/2))))
			cl$Omega <- quote(ciarray(object$mcmc.draws$Omega, level = level))
			cl$Sigma <- quote(ciarray(object$mcmc.draws$Sigma, level = level))
			if (object$fmodel >= 2) {
				cl$R <- quote(ciarray(object$mcmc.draws$R, level = level))
				if (object$fmodel == 4) {
					cl$delta <- quote(ciarray(object$mcmc.draws$delta, level = level))
					cl$Rho <- quote(ciarray(object$mcmc.draws$Rho, level = level))
				} else if (object$fmodel == 5) {
					cl$delta <- quote(apply(object$mcmc.draws$delta, 1, function(xx) quantile(xx, c(sig.level/2, 1 - sig.level/2))))
					cl$Rho <- quote(ciarray(object$mcmc.draws$Rho, level = level))
					cl$Sigma0 <- quote(ciarray(object$mcmc.draws$Sigma0, level = level))
				}
			}
		}
		return(eval(cl[[parm]]))
	}
}

