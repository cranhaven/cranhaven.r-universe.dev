#' get the highest posterior density (HPD) interval
#' @param object the output model from fitting a (network) meta analysis/regression model
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the probability which the HPD interval will cover
#' @param HPD a logical value indicating whether HPD or equal-tailed credible interval should be computed; by default, TRUE
#' @return dataframe containing HPD intervals for the parameters
#' @export
#' @method hpd bayesnmr
"hpd.bayesnmr" <- function(object, parm, level = 0.95, HPD = TRUE) {
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
	if (missing(parm)) {
		out <- list()
		if (HPD) {
            out$theta <- mhpd(theta.post, level)
			out$phi <- mhpd(object$mcmc.draws$phi, level)
			out$gam <- mhpd(object$mcmc.draws$gam, level)
			out$sig2 <- mhpd(object$mcmc.draws$sig2, level)
			out$Rho <- hpdarray(object$mcmc.draws$Rho, level = level)
			if (object$control$sample_df) {
				out$df <- vhpd(object$mcmc.draws$df, level)
			}
			attr(out, "type") <- "HPD"
		} else {
			sig.level <- 1 - level
			out$theta <- apply(theta.post, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2)))
			out$phi <- apply(object$mcmc.draws$phi, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2)))
			out$gam <- apply(object$mcmc.draws$gam, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2)))
			out$sig2 <- apply(object$mcmc.draws$sig2, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2)))
			out$Rho <- ciarray(object$mcmc.draws$Rho, level = level)
			if (object$control$sample_df) {
				out$df <- quantile(object$mcmc.draws$df, c(sig.level/2, 1 - sig.level/2))
			}
			attr(out, "type") <- "equal-tailed CI"
		}
		class(out) <- "bayesnmr.hpd"
		return(out)
	} else {
		cl <- list()
		if (HPD) {
			cl$theta <-quote(mhpd(theta.post, level))
			cl$phi <- quote(mhpd(object$mcmc.draws$phi, level))
			cl$gam <- quote(mhpd(object$mcmc.draws$gam))
			cl$sig2 <- quote(mhpd(object$mcmc.draws$sig2))
			cl$Rho <- quote(hpdarray(object$mcmc.draws$Rho, level = level))
			if (object$control$sample_df) {
				cl$df <- quote(vhpd(object$mcmc.draws$df, level))
			}
		} else {
			sig.level <- 1 - level
			cl$theta <- quote(apply(theta.post, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2))))
			cl$phi <- quote(apply(object$mcmc.draws$phi, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2))))
			cl$gam <- quote(apply(object$mcmc.draws$gam, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2))))
			cl$sig2 <- quote(apply(object$mcmc.draws$sig2, 1, function(xx) quantile(xx, prob = c(sig.level/2, 1 - sig.level/2))))
			cl$Rho <- quote(ciarray(object$mcmc.draws$Rho, level = level))
			if (object$control$sample_df) {
				cl$df <- quote(quantile(object$mcmc.draws$df, c(sig.level/2, 1 - sig.level/2)))
			}
		}
		return(eval(cl[[parm]]))
	}
}
