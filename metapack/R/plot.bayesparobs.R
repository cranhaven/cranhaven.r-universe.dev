#' get goodness of fit 
#' @param x the output model from fitting a meta analysis/regression model
#' @param ... additional parameters for plot
#' @importFrom graphics par
#' @importFrom stats density 
#' @return No return value
#' @export
#' 
#' 
'plot.bayesparobs' <- function(x, ...) {
	nkeep <- x$mcmc$nkeep
	param <- x$mcmc.draws$theta
	p <- nrow(param)
	if (x$scale_x) {
		J <- ncol(x$Outcome)
		xcols <- ncol(x$XCovariate)
		tlength <- nrow(x$mcmc.draws$theta)
		trlength <- tlength - xcols * J
		tscale <- c(rep(apply(x$XCovariate, 2, sd), J), rep(1, trlength))
	} else {
		tlength <- nrow(x$mcmc.draws$theta)
		tscale <- rep(1, tlength)
	}
	theta.post <- vapply(1:x$mcmc$nkeep, function(ikeep) {
		param[,ikeep] / tscale
	}, FUN.VALUE = numeric(tlength))
	wname <- rownames(param)	
	if (p == 2) {
		old_pars <- par(mfcol = c(2, 2))
		on.exit(par(old_pars))
		for (i in 1:p) {
			plot(1:nkeep, param[i, ], xlab = "Iteration", ylab = "", main = wname[i], type = "l")
			plot(density(param[i, ]), main = "")
		}
	} else if (p == 3) {
		old_pars <- par(mfcol = c(2, 3))
		on.exit(par(old_pars))
		for (i in 1:p) {
			plot(1:nkeep, param[i, ], xlab = "Iteration", ylab = "", main = wname[i], type = "l")
			plot(density(param[i, ]), main = "")
		}
	} else if (p == 4) {
		old_pars <- par(mfcol = c(2, 2))
		on.exit(par(old_pars))
		for (i in 1:p) {
			plot(1:nkeep, param[i, ], xlab = "Iteration", ylab = "", main = wname[i], type = "l")
			plot(density(param[i, ]), main = "")
		}
	} else {
		old_pars <- par(mfcol = c(2, 3))
		on.exit(par(old_pars))
		for (i in 1:p) {
			plot(1:nkeep, param[i, ], xlab = "Iteration", ylab = "", main = wname[i], type = "l")
			plot(density(param[i, ]), main = "")
		}
	}
}
