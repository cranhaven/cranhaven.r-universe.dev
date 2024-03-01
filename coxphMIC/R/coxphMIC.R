#' Sparse Estimation for a Cox PH model via Approximated Information Criterion
#'
#' @name coxphMIC
#' @aliases coxph.MIC
#' @aliases coxPHMIC
#' @param formula A formula object, with the response on the left of a \code{~} operator, and the terms on the right.
#' The response must be a survival object as returned by the \code{Surv} function.
#' @param data A data.frame in which to interpret the variables named in the \code{formula} argument.
#' @param method.beta0 A method to supply the starting point for beta with choices: \code{"MPLE"} and "\code{ridge}".
#' By default, the maximum partial likelihood estimator (MPLE) is used with "\code{MPLE}". The option "\code{ridge}"
#' asks for a ridge estimator with penalty parameter specified by \code{theta0}. You may supply a set of values for beta0 of
#' your choice. If \code{NULL}, then beta0 is set as 0.
#' @param beta0 User-supplied beta0 value, the starting point for optimization.
#' @param theta0 Specified the penalty parameter for the ridge estimator when \code{method.beta0="ridge"}.
#' @param method Specifies the model selection criterion used. If \code{"AIC"}, the complexity penalty parameter (lambda)
#' equals 2; if \code{"BIC"}, lambda equals ln(n0), where n0 denotes the number of uncensored events. You may specify
#' the penalty parameter of your choice by setting \code{lambda0}.
#' @param lambda0 User-supplied penalty parameter for model complexity. If \code{method="AIC"} or \code{"BIC"}, the value
#' of \code{lambda0} will be ignored.
#' @param a0 The scale (or sharpness) parameter used in the hyperbolic tangent penalty. By default, \code{a0} is set as
#' n0, where n0 is again the number of uncensored events.
#' @param scale.x Logical value: should the predictors X be normalized? Default to \code{TRUE}.
#' @param maxit.global  Maximum number of iterations allowed for the global optimization algorithm -- \code{SANN}. Default value is 300.
#' @param maxit.local Maximum number of iterations allowed for the local optimizaiton algorithm -- \code{BFGS}. Default value is 100.
#' @param rounding.digits Number of digits after the decimal point for rounding-up estiamtes. Default value is 4.
#' @param zero Tolerance level for convergence. Default is \code{sqrt(.Machine$double.eps)}.
#' @param compute.se.gamma Logical value indicating whether to compute the standard errors for gamma in the
#' reparameterization. Default is \code{TRUE}.
#' @param compute.se.beta Logical value indicating whether to compute the standard errors for nonzero beta estimates.
#' Default is \code{TRUE}. Note that this result is subject to post-selection inference.
#' @param CI.gamma Logical indicator of whether the confidence inverval for \code{gamma} is outputed. Default is \code{TRUE}.
#' @param conf.level Specifies the confidence level for \code{CI.gamma}. Defaulted as 0.95.
#' @param details Logical value: if \code{TRUE}, detailed results will be printed out when running \code{coxphMIC}.
#' @details
#' The main idea of MIC involves approximation of the l0 norm with a continuous or smooth
#' unit dent function. This method bridges the best subset selection and regularization by
#' borrowing strength from both. It mimics the best subset selection using a penalized likelihood
#' approach yet with no need of a tuning parameter.
#'
#' The problem is further reformulated with a reparameterization step by relating \code{beta}
#' to \code{gamma}. There are two benefits of doing so:  first, it reduces the optimization to
#' one unconstrained nonconvex yet smooth programming problem, which can be solved efficiently
#' as in computing the maximum partial likelihood estimator (MPLE); furthermore, the
#' reparameterization tactic yields an additional advantage in terms of circumventing post-selection inference.
#' Significance testing on \code{beta} can be done through \code{gamma}.
#'
#' The solve the smooth yet nonconvex optimization, a simulated annealing (\code{method="SANN"} option
#' in \code{\link[stats]{optim}}) global optimization algorithm is first applied. The resultant estimator is then used
#' as the starting point for another local optimization algorithm. The quasi-Newton BFGS method (\code{method="BFGS"}
#' in \code{\link{optim}}) is used.
#'
#' In its current version, some appropriate data preparation might be needed. For example, nomincal
#' variables (especially character-valued ones) needed to be coded with dummy variables; missing values would cause
#' errors too and hence need prehanlding too.
#'
#' @return A list containing the following component is returned.
#' \describe{
#' \item{opt.global}{Results from the preliminary run of a global optimization procedure (\code{SANN} as default).}
#' \item{opt.local}{Results from the second run of a local optimization procedure (\code{BFGS} as default).}
#' \item{min.Q}{Value of the minimized objective function.}
#' \item{gamma}{Estimated gamma;}
#' \item{beta}{Estimated beta;}
#' \item{VCOV.gamma}{The estimated variance-covariance matrix for the gamma estimate;}
#' \item{se.gamma}{Standard errors for the gamma estimate;}
#' \item{se.beta}{Standard errors for the beta estimate (post-selection);}
#' \item{BIC}{The BIC value for the \emph{selected} model;}
#' \item{result}{A summary table of the fitting results.}
#' \item{call}{the matched call.}
#' }
#' @examples
#'   # PREPARE THE PBC DATA
#'   library(survival); data(pbc);
#'   dat <- pbc; dim(dat);
#'   dat$status <- ifelse(pbc$status==2, 1, 0)
#'   # HANDLE CATEGORICAL VARIABLES
#'   dat$sex <- ifelse(pbc$sex=="f", 1, 0)
#'   # LISTWISE DELETION USED TO HANDLE MISSING VALUES
#'   dat <- stats::na.omit(dat);
#'   dim(dat); utils::head(dat)
#'
#'   fit.mic <- coxphMIC(formula=Surv(time, status)~.-id, data=dat, method="BIC", scale.x=TRUE)
#'   names(fit.mic)
#'   print(fit.mic)
#'   plot(fit.mic)
#' @seealso \code{\link{coxph}}
#' @references
#'\itemize{
#' \item Abdolyousefi, R. N. and Su, X. (2016). \bold{coxphMIC}: An R package for sparse estimation of Cox PH Models via approximated information criterion. Tentatively accepted, \emph{The R Journal}.
#' \item Su, X. (2015). Variable selection via subtle uprooting.
#' \emph{Journal of Computational and Graphical Statistics}, \bold{24}(4): 1092--1113.
#' URL \url{http://www.tandfonline.com/doi/pdf/10.1080/10618600.2014.955176}
#' \item Su, X., Wijayasinghe, C. S., Fan, J., and Zhang, Y. (2015). Sparse estimation of Cox proportional
#' hazards models via approximated information criteria. \emph{Biometrics}, \bold{72}(3): 751--759.
#' URL \url{http://onlinelibrary.wiley.com/doi/10.1111/biom.12484/epdf}
#' }
#' @import numDeriv survival
#' @export


coxphMIC <- function(formula=Surv(time, status)~., data,
	method.beta0="MPLE", beta0=NULL, theta0=1,  		# INTITAL VALUES FOR BETA; theta0 IS THE PENALTY PARAMETER FOR RIDGE REGRESSION
	method="BIC", lambda0=2, 					# OPTIONS FOR LAMBDA
	a0=NULL, scale.x=TRUE,
	maxit.global = 300,  maxit.local = 100, rounding.digits = 4, zero=sqrt(.Machine$double.eps),
	compute.se.gamma=TRUE, compute.se.beta=TRUE, CI.gamma=TRUE, conf.level=0.95,
	details=FALSE)
{
	# PREPARE time, status, AND X MATRIX
	# --------------------------------------------
	if (missing(data))  data <- environment(formula)
    	Call <- match.call()
     	indx <- match(c("formula", "data"), names(Call), nomatch = 0)
	if (indx[1] == 0) stop("The formula= argument is required.")
    	temp <- Call[c(1, indx)]
    	temp[[1]] <- as.name("model.frame")

    	if (is.R()) m <- eval(temp, parent.frame())
    	else m <- eval(temp, sys.parent())
    	Terms <- stats::terms(m)

	var_names <- all.vars(formula)	# extract the variable names from the fomula
	time <- data[,var_names[1]];
	status <- data[,var_names[2]]

    	attr(Terms, "intercept") <- NULL ########## NO INTERCEPT
    	X <- stats::model.matrix(Terms, m)
	# print(utils::head(X))

	if (is.R()) {
      	assign <- lapply(attrassign(X, Terms)[-1], function(x) x - 1)
        	xlevels <- stats::.getXlevels(Terms, m)
    	}
    	else {
        assign <- lapply(attr(X, "assign")[-1], function(x) x -1)
        xvars <- as.character(attr(Terms, "variables"))
        xvars <- xvars[-attr(Terms, "response")]
        if (length(xvars) > 0) {
            xlevels <- lapply(m[xvars], levels)
            xlevels <- xlevels[!unlist(lapply(xlevels, is.null))]
            if (length(xlevels) == 0)
                xlevels <- NULL
        }
        else xlevels <- NULL
	}
    	Xnames <- colnames(X)
	p <- NCOL(X); n <- NROW(X)
	if (details) print(utils::head(X))

	# SORT TIME ACCORDING TO THE OBSERVED FAILURE TIMES IN **DECREASING** ORDER.
	# --------------------------------------------------------------------------
	X <- X[order(time, decreasing =T), ]
	status <- status[order(time, decreasing =T)]
	time <- sort(time, decreasing=T)
	n0 <- sum(status==1);    	# n0 -- NUMBER OF DEATHS
	if (is.null(a0)) a0 <- n0 	#  SET a0 = n0 IF a0 IS NOT GIVEN OTHERWISE.

	# SCALE OR STANDARDIZE THE DATA SET IF NOT DONE.
	if (scale.x) X <- scale(X, center = TRUE, scale = TRUE)
	dat <- data.frame(cbind(time, status, X))  # FORM NEW DATASET
	######################### TO AVOID ERROR WHEN HAVING ~.-id IN FORMULA
	formula0 <- stats::as.formula(paste(c("Surv(time, status)~", Xnames), collapse=" + "))


	# OBTAIN INITIAL VALUES FOR BETA
	# ----------------------------------
	if (method.beta0=="MPLE") {beta0 <- coxph(formula0, data=dat)$coef}
	else if (method.beta0=="ridge") {
		form <- stats::as.formula(paste("Surv(time, status) ~ ridge(",
			c(paste(Xnames, ", ", sep="", collapse="")), "theta=", theta0, ")", sep=""))
		beta0 <- as.vector(coxph(form, data=dat)$coef)
	}
	else if (!is.null(beta0)) beta0 <- beta0
	else beta0 <- rep(0, p)
	if (details) print(beta0)

	# COMPUTE LAMBDA
	# --------------------
	if (method=="AIC") lambda <- 2
	else if (method=="BIC") {lambda <- log(n0)}
	else if(!is.null(lambda0)) lambda <- lambda0
	else stop("Please provide a value for lambda.")

	# OPTIMIZATION OF THE OBJECTIVE FUNCTION
	# --------------------------------------------
	# THE PENALIZED PARTIAL LOG-LIKELIHOOD AND GRADIENT
	fun <- LoglikPen
	grad <- NULL

	# OPTIMIZATION USING SIMULATED ANNEALING (GLOBAL), FOLLOWED BY BFGS (LOCAL)
	opt.fit1 <- stats::optim(par=beta0, fn=fun, gr = grad,
      	method = "SANN", control = list(maxit=maxit.global, trace=F, reltol=zero),
		time=time, status=status, X=X, lambda=lambda, a=a0)
	beta1 <- opt.fit1$par; #
	if (details) print(beta1)
	opt.fit2 <- stats::optim(par=beta1, fn=fun, gr = grad,
		method = "BFGS", control = list(maxit=maxit.local, trace=F, reltol=zero),
		time=time, status=status, X=X, lambda=lambda, a=a0)
	beta2 <- opt.fit2$par
	if (details) print(beta2)
	min.Q <- opt.fit2$value
	# CHECK CONVERGENCE.
	converge <- ifelse(opt.fit2$convergence==0, T, F)
	# PREPARE THE OUTPUT
	beta2 <- as.double(beta2);
	w.hat <- tanh(a0*beta2^2)
	beta.hat <- beta2*(w.hat)
	names(beta.hat) <- Xnames
	if (details) print(beta.hat)
	beta.SU <- round(beta.hat, rounding.digits)

	result <- cbind(beta0=round(beta0, rounding.digits),
			gamma=round(beta2, rounding.digits))
	result <- as.data.frame(result)
	out <- list(opt.global=opt.fit1, opt.local=opt.fit2, min.Q=min.Q,
		gamma=beta2, beta=beta.SU)

	# COMPUTE SE FOR GAMMA (i.e., @beta2)
	# -------------------------------------
	if (compute.se.gamma) {
		fit.tmp <- coxph(formula0, data=dat, init=beta2, control=coxph.control(iter.max = 0))
		VCOV.bfbeta <- stats::vcov(fit.tmp)
		wt0 <- 1; bias <- 0

		se.bfbeta <- sqrt(diag(VCOV.bfbeta))
		result <- cbind(result, se.gamma=round(se.bfbeta, rounding.digits))
		if (CI.gamma) {
		  if (conf.level <= 0 || conf.level >=1) stop("The argument conf.level needs to within 0 and 1.")
		  sig.level <- 1-conf.level
		  z0 <- stats::qnorm(1-sig.level/2)
		  LB <- beta2 - z0*se.bfbeta
		  UB <- beta2 + z0*se.bfbeta
		  result <- cbind(result, LB=LB, UB=UB)
		}

		z.stat <- (wt0*beta2)/se.bfbeta
		p.value <- 2* stats::pnorm(abs(z.stat), lower.tail=F)
		fit.summary <- data.frame(gamma=beta2, se=se.bfbeta, z.stat=z.stat, p.value=p.value)  # BASED ON GAMMA, NOT BETA
		result <- cbind(result, z.stat=round(z.stat, rounding.digits),
			p.value=round(p.value, rounding.digits))
		out <- c(out, list(VCOV.gamma=VCOV.bfbeta, se.gamma=se.bfbeta))
	}
	result <- cbind(result, beta.MIC=beta.SU)


	# COMPUTE SE FOR BETA -- NONZERO ONES ONLY
	# -----------------------------------------
	if (compute.se.beta) {
		beta.prime.1 <- beta.hat[beta.SU!=0];
		beta.1 <- beta2[beta.SU!=0];  q <- length(beta.1)
		if (q==0) se.beta.SU <- se0.beta.SU <- rep(NA, p)   # DEAL WITH THE SCENARIO THAT ALL BETAS ARE 0.
		else {
			terms1 <- names(beta.prime.1); # print(terms1)
			form1 <- stats::as.formula(paste(c("Surv(time, status)~", terms1), collapse=" + "))
			fit1 <- coxph(form1, data=dat, init=beta.prime.1, control=coxph.control(iter.max = 0))
			BIC <- stats::BIC(fit1)

			# METHOD I: USE THE FIHER'S INFO MATRIX DIRECTLY
			se1 <- sqrt(diag(stats::vcov(fit1)))
			tmp <- rep(NA, p);# tmp <- rep(0, p);
			names(tmp) <- Xnames; tmp[names(se1)] <- se1
			se0.beta.SU	<- tmp
		}
		result <- cbind(result, se.beta.MIC=round(se0.beta.SU, rounding.digits))
		out <- c(out, list(se.beta=se0.beta.SU, BIC=BIC))
	}
	row.names(result) <- Xnames
	out <- c(out, list(result=result))
	out$call <- Call
	class(out) <- "coxphMIC"
	return(out)
}




#
