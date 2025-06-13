# Copyright (c) 2020 Andre GILLIBERT
# The licence of this file is GPLv2
# A few lines of documentation has been copy-pasted from the core R packages
# Such as the description of some parameters of summary.glm
# Otherwise, the source code is 100% original

#' glmglrt: GLRT P-Values in Generalized Linear Models
#'
#' This package has been developed to provide Generalized Likelihood Ratio Tests (GLRT)
#' also known as Likelihood Ratio Tests (LRT) to Generalized Linear Models (GLMs).
#' The \link{stats} package do support LRT P-values with \code{\link[stats:anova]{anova}} and
#' derived confidence intervals with \code{confint()}, but provides Wald's P-values with
#' the \code{\link[stats:summary.glm]{summary}} function.
#' This is unfortunate for two reasons: Wald's P-values may be inconsistent with profile-likelihood
#' confidence intervals and Wald's P-values, on small samples are more biased than LRT P-values, for
#' non-gaussian models. The \code{\link[stats:anova]{anova}} function is not as simple as
#' \code{\link[stats:summary.glm]{summary}}, since it requires manually fitting two models.
#'
#' @section Summary function:
#' This package provides a way to override (see \link{override_summary}) the standard \code{\link[stats:summary.glm]{summary.glm}} function
#' by a \code{\link{summarylr}} function that provides LRT and/or Rao's score P-values.
#'
#' @section Functions to estimate contrasts:
#' It also provides functions \code{\link{estimate_contrast}}, \code{\link{confint_contrast}} and \code{\link{p_value_contrast}}
#' to estimate contrasts of coefficients of GLMs with LRT, Rao's and Wald's hypothesis tests and confidence intervals
#' This is an alternative to \code{\link[multcomp:glht]{multcomp::glht}} without
#' Wald's approximation ! It also provides a less powerful \code{\link{p_value.glm}} method for the S3 generic
#' \link[parameters:p_value]{parameters::p_value}. It also extends this S3 generic for a variety
#' of models as \code{\link{p_value.default}}. That time, the only method supported for all models, is Wald's method.
#'
#'
#' @docType package
#' @name glmglrt
NULL

#' @importFrom stats glm.fit
#' @importFrom stats family
#' @importFrom stats gaussian
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @importFrom stats model.extract
#' @importFrom stats coef
#' @importFrom stats vcov
#' @importFrom stats pt
#' @importFrom stats qt
#' @importFrom stats df.residual
#' @importFrom stats glm.control
#' @importFrom stats glm
#' @importFrom stats anova
#' @importFrom stats weights
#' @importFrom stats pchisq
#' @importFrom stats poisson
#' @importFrom stats binomial
#' @importFrom stats qnorm
#' @importFrom stats formula
#' @importFrom stats nobs
#' @importFrom stats sd
#' @importFrom stats uniroot
#' @importFrom MASS glm.nb
#' @importFrom parameters p_value
#' @importFrom utils getS3method

sdx=function(v) {if (length(v)==1) {0} else {sd(v)}}

level_warning = 1
level_note = 2
level_debug = 3
msg=function(debuglevel,messagelevel, ...) {
	# debuglevel = actual level of debug we wish to use. The higher it is, the most info is shown.
	# debuglevel = 0 (no message), debuglevel = 1 (warnings), debuglevel = 2 (warnings+notes), debuvlevel = 3 (warnings+notes+debug info)
	#
	# messagelevel = actual level of the message we attempt to display
	# ... character parameters that are pasted together before being displayed
	if (is.null(debuglevel)) {
		stop("debuglevel is mandatory")
	}

	if (debuglevel >= messagelevel) {
		if (messagelevel == level_warning) {
			warning(paste(..., collapse=" "))
		} else if (messagelevel %in% c(level_note, level_debug)) {
			message(paste(..., collapse=" "))
		} else {
			stop(paste0("unknown message level ", messagelevel))
		}
	}
}
warn=function(debuglevel, ...) {
	msg(debuglevel, level_warning, ...)
}
note=function(debuglevel, ...) {
	msg(debuglevel, level_note, ...)
}

add_warnings=function(object, smod) {
	rel = try(is_relative_family(family(object)),silent=TRUE)
	if (inherits(rel, "try-error")) {
		rel=FALSE
		if (inherits(object, "polr")) {
			rel=TRUE
		}
	}
	coefs = fixcoef(object)
	coef_ok = !is.na(coefs)
	
	vc = vcov_fixcoef(object)
	
	mm = try(model.matrix(object), silent=TRUE)
	if (inherits(mm, "try-error")) { # not applicable (e.g. nls)
		return(smod)
	}

	nms = intersect(names(coefs), colnames(mm))
	idxvc = match(nms, names(coefs))
	vc = vc[idxvc,idxvc,drop=FALSE]
	idxmm = match(nms, colnames(mm))
	mm = mm[,idxmm,drop=FALSE]

	sdexpo = apply(mm, 2, sdx)
	
	if (is.list(object) && !is.null(object$converged) && is.logical(object$converged) && !object$converged) {
		# To reduce the risk of backward incompatibility
		# 1) Do not add the field in most common cases (convergence successful)
		# 2) Make sure that the first letter of the field is discriminative to allow scripts that only refer
		#    to the first letters of an old field (e.g. summarylr(model)$con for summarylr(model)$contrasts)
		smod$extra$problem_of_convergence = "general"
	} else if (all((vc*sdexpo)[!is.na(vc)] > 50^2) & length(sdexpo) > 0 & rel) {
		smod$extra$problem_of_convergence = "all"
	} else if (any((vc*sdexpo)[!is.na(vc)] > 50^2) & rel) {
		smod$extra$problem_of_convergence = "specific"
	}

	return(smod)
}

get_fit_method=function(object) {
	if (inherits(object, "negbin")) {
		link = family(object)$link
		fit=function(x, y, weights=rep(1, nrow(x)), start=NULL, etastart=NULL, mustart=NULL,
			offset=rep(0, nrow(x)), family=poisson(log), control=list(), intercept=TRUE, singular.ok=TRUE) {
			off=offset
			params = list(link=family$link,y ~ 0+x+offset(off), weights=weights, control=control, start=start, etastart=etastart,mustart=mustart,method=object$method)
			if (ncol(x) == 0) {
				params[[2]]=y ~ 0+offset(off)
			}
			return(do.call(MASS::glm.nb,params))
		}
		return(fit)
	}
	fit = object$method
	if (is.character(fit)) {
		fit = get(fit,mode="function")
	} else if (!is.function(fit)) {
		fit = stats::glm.fit
	}
	return(fit)
}

#' Gets the degree of freedom for Wald tests involving the model
#'
#' This generic function is used by \code{\link{p_value_contrast.default}} to
#' get the number of degrees of freedom of the \link[stats:TDist]{t distribution} that
#' approximates the point estimate of the contrast divided by its standard error.
#'
#' @param object statistical model;
#' @param ... Unused by \code{p_value_contrast.default}, but may be useful to some custom specializations.
#'
#' @return
#' A finite value or Inf for normal distribution approximation.
#' @details
#' This function is quite similar to \code{\link[stats:df.residual]{df.residual}} but it should return Inf when the Student's t
#' distribution is less appropriate than the normal distribution.
#' @family Wald-related functions
#' @examples
#' # 10 observations, one coefficient, 9 degrees of freedom
#' df_for_wald(glm(I(1:10) ~ 1))
#' # returns Inf (non-gaussian-identity model)
#' df_for_wald(glm(family="poisson", c(10,20,30) ~ 1))
#' data(mtcars)
#' # returns Inf (non-gaussian-identity model)
#' df_for_wald(glm(family="binomial", data=mtcars, I(hp > median(hp)) ~ cyl))
#' @export
df_for_wald=function(object, ...) {
	UseMethod("df_for_wald")
}

#' @describeIn df_for_wald Returns \code{\link[stats:df.residual]{df.residual}} for linear gaussian models and Inf
#' for all other models in order to make Wald's tests consistent with the behavior of \code{\link[stats:summary.glm]{stats::summary.glm(object)}}
#' @export
df_for_wald.glm=function(object, ...) {
	if (family(object)$family %in% c("gaussian", "Gamma", "inverse.gaussian")) {
		dfr = df.residual(object)
	} else {
		dfr = Inf
	}
	return(dfr)
}

#' @describeIn df_for_wald Simple proxy to \code{\link[stats:df.residual]{df.residual}} but replaces NAs with Inf
#' @export
df_for_wald.default=function(object, ...) {
	if (inherits(object, "lme") && is.list(object) && is.list(object$fixDF) && is.vector(object$fixDF$X) && is.numeric(object$fixDF$X)) {
		dfr = max(object$fixDF$X)
		return (dfr)
	}
	if (inherits(object, "polr")) {return(Inf)}
	dfr = df.residual(object)
	if (is.null(dfr) || is.na(dfr)) {dfr=Inf} # for rlm
	dfr
}


pvalues_glm_wald=function(object, parm, H0, alternative) {
		SE = sqrt(diag(vcov_fixcoef(object))[parm])
		z  = (coef(object)[parm] - H0) / SE
		dfr = df_for_wald(object)
		if (alternative == "two.sided") {
			p = 2*pt(-abs(z),df=dfr)
		} else {
			p =   pt(z,df=dfr,lower.tail=(alternative=="less"))
		}
		return(p)

}

make.strong.refit=function(object, coef_name, debuglevel, reserve_start) {
strong.refit=function(x, y, weights = NULL,
	start = NULL, etastart = NULL, mustart = NULL,
	offset = NULL, family = gaussian(),
	control = list(), intercept = FALSE, singular.ok = TRUE) {
	
	# defaulting to intercept=FALSE fixes bug PR#17735 in R for version < 4.0.0 (Rao test without intercept for stats::anova.glm)
	
	out = strong_fit(object, y, x, ini_weights=weights, off=offset, coef_name=coef_name, debuglevel=debuglevel, reserve_start=reserve_start, fam=family, intercept=intercept)
	if (is.null(out) || inherits(out, "try-error")) {
		stop("strong.refit: failed to converge")
	}
	out
}
return(strong.refit)
}

strong_glm=function(object, y, mmx, ini_weights, off, coef_name, debuglevel=1, reserve_start=NULL, control=object$control) {
	if (is.null(reserve_start)) {
		stop("reserve_start is mandatory")
	}
	if (inherits(object, "negbin")) {
		out = try(strong_fit(object, y, mmx, ini_weights, off, coef_name, debuglevel=debuglevel, reserve_start=reserve_start), silent=TRUE)
	} else {
		if (ncol(mmx)==0) {formx=y ~ 0} else {formx = y~0+mmx}
		out = try(glm(family=family(object), formula=formx, weights=ini_weights, offset=off, start=reserve_start, control=control, method=make.strong.refit(object, coef_name, debuglevel, reserve_start)),silent=TRUE)
	}
	if (inherits(out, "try-error")) {
		return(list(converged=FALSE, family=family(object)))
	}
	out
}

# the following function refits a glm
# with a different response and model matrix (e.g. one less coefficient)
# it is based on object$fit
# but, it finds "good" starting values if there are convergence problems
# it returns a fitted glm
# reserve_start must contain a vector of length ncol(mmx) containing start values
# that have been got from coef(object)
strong_fit=function(object, y, mmx, ini_weights, off, coef_name, debuglevel=1, reserve_start=NULL, fam=family(object), intercept=FALSE, control=object$control) {
	fit = get_fit_method(object)
	logbinom = fam$link == "log" & fam$family == "binomial"

	control$trace = FALSE

	start = NULL

	if (is.null(reserve_start)) {stop("reserve_start is mandatory")}

	newfit = try(fit(mmx, y,
			offset=off,
			family=fam,
			intercept=intercept,
			weights=ini_weights,
			control=control
		),silent=T)

	conv_failed = (inherits(newfit, "try-error") && grepl("no valid set of coefficients", newfit)) || (!inherits(newfit, "try-error") && !newfit$converged)
	if (conv_failed & logbinom) {
		note(debuglevel, "coefficient", coef_name, ": trying different starting values for log-binomial model.\n")
		# log-binomial model failed to converge.
		# we calculate better starting values that keep predicted values as close as possible to 0.50
		# in order to keep inside the response space
		startfit = try(fit(mmx, y=rep(0.5, nrow(mmx)),
			offset=off,
			family=gaussian(log),
			intercept=intercept,
			weights=ini_weights,
			control=control, start=rep(0,ncol(mmx))),silent=T)
		if (inherits(startfit, "try-error")) {
			warn(debuglevel,"coefficient", coef_name, ": no acceptable starting values found for log-binomial model.\n")
			return(NULL)
		}
		start = startfit$coefficients
		newfit = try(fit(mmx, y,
			offset=off,
			family=fam,
			intercept=intercept,
			weights=ini_weights,
			control=control, start=start),silent=T)
		return(newfit)
	} else if (conv_failed) {
		note(debuglevel,"coefficient", coef_name, ": trying starting values based on H1 model to fit the H0 model.\n")
		newfit = try(fit(mmx, y,
			offset=off,
			family=fam,
			intercept=intercept,
			weights=ini_weights,
			control=control, start=reserve_start),silent=T)
	}

	return(newfit)
}

# rel=TRUE indicates that coefficients are relative
# so that a major variance of vcov(newfit) means that newfit has diverged
# actually, the variance has to be divided by the standard deviation of the coefficient
# for quantitative variables
verify_fit=function(newfit, mmx, coef_name="", debuglevel=1, force=FALSE, rel=FALSE) {
	if (inherits(newfit, "try-error")) { # typically convergence fails when removing (intercept) in log-binomial models
		warn(debuglevel,"coefficient", coef_name, ": Hypothesis test failed because the H0 model did not converge.\n")
		return(NULL)
	}
	if (!newfit$converged & !force) {
		warn(debuglevel,"coefficient", coef_name, ": Hypothesis test failed because the H0 model diverged.\n")
		return(NULL)
	} else if (!newfit$converged) {
		warn(debuglevel,"coefficient", coef_name, ": the H0 model diverged but the hypothesis test will be calculated (force=TRUE).\n")
	}
	if (any(is.na(newfit$coefficients))) {
		warn(debuglevel,"coefficient", coef_name, ": Hypothesis test not done because the H0 model has one or more non-estimable coefficients.\n")
	}
	if (any(diag(vcov(newfit))*apply(mmx,2,sdx) > 50^2) & rel & !force) { # divergence manifeste
		warn(debuglevel,"coefficient", coef_name, ": Hypothesis test not done because the H0 model has one or more non-estimable coefficients (huge standard error).\n")
		return(NULL)
	}
	return(newfit)
}

pvalue_from_two_models=function(object1, object2, method=c("LRT","Rao","Chisq","F"), debuglevel=1, force=FALSE) {
	method = method[1]

	if (method %in% c("Chisq", "LRT") && inherits(object1, "negbin")) {
		method="Chisq"
	} else if (inherits(object1, "negbin")) {
		stop("Only LRT is supported by negative binomial regression")
	}
	if (family(object1)$link != family(object2)$link) {
		# sanity check
		if (!force) {
			stop("inconsistency: different link function for the two models to compare")
		}
	}
	a = stats::anova(object1, object2, test=method)
	if (method == "F") {
		pvalue=a[["Pr(>F)"]][2]
		if (is.na(pvalue) & a[2,"Deviance"]<1e-10) {
			return(1)
		} else if (is.na(pvalue)) {
			note(debuglevel,"anova(test=\"F\") returned NA for P-value")
			return(NA)
		}
	} else {
		pvalue = a[["Pr(>Chi)"]][2]
		if (is.null(pvalue)) {
			pvalue = a[["Pr(Chi)"]][2]
		}
		if (force & (is.na(pvalue) | is.null(pvalue))) {
			pvalue = pchisq(-a[2,"Deviance"],df=-a[2,"Df"],lower.tail=F)
		}
		if (is.na(pvalue)) {
			note(debuglevel,paste0("anova(test=\"", method, "\") returned NA for P-value"))
		}
	}

	return(pvalue)
}

# This function converts a two-sided P-value to a two-sided or one-sided P-value
# Given an estimate of the effect to compare to zero
twosided2pvalue=function(pvalue, estimate, alternative=c("two.sided","less","greater")) {
	if (is.na(estimate) | is.na(pvalue)) {return(NA)}
	alternative = alternative[1]
	if (alternative == "two.sided") {return(pvalue)}
	else if (alternative %in% c("less","greater")) {
		H1_direction = ifelse(alternative == "less", -1, 1)*sign(estimate) >= 0
		# if estimate == 0, direction does not matter (P-value = 0.5)
		pvalue=pvalue*0.5
		if (!H1_direction) {pvalue = 1-pvalue}
		return(pvalue)
	} else {
		stop("alternative must be two.sided, less or greater")
	}
}

get_coef_name=function(object, contrasts) {
	multiplier=ifelse(contrasts==1,"",ifelse(contrasts==-1,"-",paste0(contrasts, "*")))
	coef_name = paste(paste0(multiplier, names(coef(object)))[contrasts != 0],collapse=" + ")
	return(coef_name)
}

# computes LRT one p-value from a glm
# object is the glm
# y as the response vector/matrix
# mm as the model matrix
# parm as the set of parameters from which a P-value must be computed
# xcoef is the vector of coefficients of the initial model
# vc is the vcov matrix of the initial model
# ini_weights a vector of initial weights of the model that where passed as the weights argument of glm()
# off a vector of offsets of the model
# you must make sure that
# length(off) == length(ini_weights) == nrow(mm)
# length(y) == nrow(mm) or nrow(y) == nrow(mm)
# fit is a function (not a character string)
# contrasts is a vector of length xcoef representing a contrast to test
# unestimable effects must have been removed from mm
# H0 represents the value of the contrast under H0

pvalue_glm_from_mm=function(object, y, mm, xcoef, vc, ini_weights, off, contrasts=NULL, H0=0, alternative=c("two.sided","less","greater"), method=c("LRT","Rao","Chisq","F"), force=FALSE, debuglevel=1) {
	fam = family(object)
	method = method[1]

	if (!(method %in% c("LRT","Rao","Chisq","F"))) {
		stop("unsupported test method")
	}

	if (length(contrasts) != length(xcoef)) {
		stop("contrasts and xcoef must have the same length")
	}

	if (!(is.vector(contrasts) & is.numeric(contrasts) && all(is.finite(contrasts)))) {
		stop("contrast must be finite vector")
	}
	if (all(contrasts == 0)) {
		stop("contrast must not be all-zero")
	}
	coef_name = get_coef_name(object, contrasts)

	logbinom = fam$link == "log" & fam$family == "binomial"
	rel = fam$link %in% c("logit", "log") & fam$family %in% c("poisson", "binomial")

	#is_intercept = (sdexpo == 0)
	SE_new_coef = sqrt(as.vector(t(contrasts) %*% vc %*% contrasts))
	SD_new_vari = sdx(mm %*% contrasts)
	if (SE_new_coef*SD_new_vari > 50 & rel & !force) {
		warn(debuglevel,"coefficient", coef_name, "has too large standard error. LRT not done.\n")
		return(NA)
	} # convergence problem
	if (!object$converged & !force) {
		warn(debuglevel,"coefficient", coef_name, ": LRT not done because the H1 model did not converge.\n")
		return(NA)
	} # global object did not converge

	itarget = which.max(contrasts)

	oldfit = strong_glm(object, y=y, mmx=mm, ini_weights=ini_weights, off=off, coef_name=coef_name, debuglevel=debuglevel, reserve_start=xcoef, control=glm.control(epsilon=1e-8, maxit=1, trace=FALSE))

	matd = matrix(nrow=nrow(mm), ncol=ncol(mm), contrasts/contrasts[itarget], byrow=TRUE)
	matt = matrix(nrow=nrow(mm), ncol=ncol(mm), mm[,itarget], byrow=FALSE)
	varcomp = rowSums(mm * matd)
	matd[,itarget] = 0
	mm = mm - matd * matt
	mm[,itarget] = mm[,itarget]/contrasts[itarget]

	mmx = mm[,-itarget,drop=F]
	off = off + H0*mm[,itarget]

	rcoef = xcoef[-itarget]
	coef_name = get_coef_name(object, contrasts)
	newfit = strong_glm(object, y=y, mmx=mmx, ini_weights=ini_weights, off=off, coef_name=coef_name, debuglevel=debuglevel, reserve_start=rcoef)
	newfit = verify_fit(newfit, mmx, coef_name=coef_name, debuglevel=debuglevel, force=force, rel=rel)

	if (is.null(newfit)) {return(NA)}

	# force=TRUE is required even in standard scenarii as soon as the P-value is extremely close to 1
	# This happens in test scenarii where a coefficient is compared to itself
	# In real life, this may happen with binary variables where groups are perfectly balanced
	
	pvalue = pvalue_from_two_models(oldfit, newfit, method=method, debuglevel=debuglevel, force=TRUE)
	estimate = sum(xcoef * contrasts) - H0
	pvalue = twosided2pvalue(pvalue, estimate, alternative)
	return(pvalue)
}

# works like pvalue_glm_from_mm
# but requires fewer parameters
# it finds unestimable coefficients (dropped from model matrix)
# and removes them from all objects
# then, call pvalue_glm_from_mm
pvalue_glm_contrast_lrt=function(object, contrasts, H0=0, alternative=c("two.sided","less","greater"), method=c("LRT","Rao","Chisq","F"), force=FALSE, debuglevel=1) {
	mm = model.matrix(object)
	y = model.extract(model.frame(object), "response")

	if (length(contrasts)!=length(coef(object))) {
		stop(paste0("contrasts (length ", length(contrasts), ") must have the same length as coef (length ", length(coef(object)), ") "))
	}
	if (is.vector(y) && nrow(mm) != length(y)) {
		stop("inconsistent sample sizes between response and model matrix")
	}
	if (is.matrix(y) && nrow(mm) != nrow(y)) {
		stop("inconsistent sample sizes between response and model matrix")
	}
	coef_ok = !is.na(coef(object))
	# dropped coefficients are removed from vc (variance-covariance matrix) and mm (model matrix)
	vc = vcov(object)[coef_ok,coef_ok,drop=FALSE]
	mm = mm[,coef_ok,drop=FALSE]
	xcoef = coef(object)[coef_ok]

	if (any(contrasts[!coef_ok] != 0)) {
		return(NA)
	}
	xcontrasts = contrasts[coef_ok]

	pvalue_glm_from_mm(object=object, y=y, mm=mm, xcoef=xcoef, vc=vc,
			ini_weights=get_ini_weights(object, y),
			off=get_ini_offset(object),
			contrasts=xcontrasts,
			H0=H0, alternative=alternative, method=method, force=force, debuglevel=debuglevel);
}

index2contrast=function(object, i) {
	if (!inherits(object, "glm")) {stop("object must be glm in index2contrast")}
	xcoef = coef(object)
	contrasts=rep(0, length(xcoef))
	contrasts[i]=1
	names(contrasts)=names(xcoef)
	return(contrasts)
}

get_ini_weights=function(object, y=NULL) {
	if (is.null(y)) {y=model.extract(model.frame(object), "response")}
	ini_weights = weights(object)
	if (is.null(ini_weights)) {ini_weights=rep(1, nobs(object))}
	if (family(object)$family == "binomial"  & is.matrix(y)) {
		rs = rowSums(y) # some subgroups may have zero observation
		ini_weights = ini_weights/rs
		ini_weights[rs == 0]=mean(ini_weights[is.finite(ini_weights)])
	}
	return(ini_weights)
}
get_ini_offset=function(object) {
	if (is.null(object$offset)) {off=rep(0, nobs(object))} else {off=object$offset}
	return(off)
}

valid_enumeration=function(val, values) {
	if (!is.character(val)) {return(FALSE)}
	if (!is.vector(val)) {return(FALSE)}
	val=val[1]
	if (!(val %in% values)) {return(FALSE)}
	return(TRUE)
}
assert_enumeration=function(val, name, values) {
	if (!valid_enumeration(val, values)) {
		stop(paste0(name, " must be one of: ", paste(values, collapse=", "), " but was ", val))
	}
}
assert_bool=function(val, name) {
	if (!is.vector(val) || !is.logical(val) || length(val) != 1 || is.na(val)) {
		stop(paste0(name, " must be TRUE or FALSE"))
	}
}
assert_debug_level=function(val, name) {
	if (!is.vector(val) || length(val) != 1 || !is.numeric(val) || is.na(val)) {
		stop("debuglevel must be a single defined integer value")
	}
}
check_pvalues_glm=function(object, parm, method, force, debuglevel, H0, alternative) {
	if (!inherits(object, "glm")) {stop("object must be a glm")}
	if (!(is.null(parm) | is.vector(parm) & (is.character(parm) | is.numeric(parm)))) {
		stop("parm must be a numeric or character vector")
	}
	if (any(is.na(parm))) {stop("parm must have no NA values")}
	if (is.numeric(parm)) {
		if (any(!is.finite(parm))) {stop("parm must have only finite values")}
		if (any(parm < 0 | parm > length(coef(object)))) {
			stop("parm has one or more indices outside of bounds")
		}
	}
	assert_enumeration(method, "method", c("LRT","Rao","Chisq","F","Wald"))
	assert_bool(force, "force")
	assert_debug_level(debuglevel)
	if (!(is.numeric(H0))) {
		stop("H0 must be numeric")
	}
	if (any(is.na(H0))) {
		stop("H0 must be have no NA values")
	}
	if (length(parm) == 0) {parm=seq_along(coef(object))}
	if (length(H0) != 1 & length(H0) != length(parm)) {
		stop("H0 must have length 1 or the same length as parm")
	}
	assert_enumeration(alternative, "alternative", c("two.sided","less","greater"))
}

# Computes several pvalues of a glm
pvalues_glm=function(object, parm=NULL, method=c("LRT","Rao","Chisq","F","Wald"), force=FALSE, debuglevel=1, H0=0, alternative=c("two.sided","less","greater")) {

	check_pvalues_glm(object, parm, method, force, debuglevel, H0, alternative)

	method = method[1]
	alternative = alternative[1]
	xcoef = coef(object)
	ncoefs = length(xcoef)
	if (length(parm)==0) {parm=seq_along(xcoef)}
	if (is.character(parm)) {
		if (any(is.na(parm))) {
			stop("parm must not contain missing values")
		}
		parm = match(parm, names(xcoef))
		if (any(is.na(parm))) {
			stop("parm refers to one or more non-existent coefficient name")
		}
	}

	if (length(H0) == 1) {H0 = rep(H0, length(parm))}
	if (length(H0) != length(parm)) {
		stop("length of H0 must be equal to length of parm")
	}
	if (method %in% c("F","Wald")) {
		return(pvalues_glm_wald(object, parm, H0, alternative))
	}

	if (family(object)$family %in% c("quasi", "quasipoisson", "quasibinomial")) {
		stop(paste0("method ", method, " unsupported for computing p-values of quasi-likelihood models"))
	}

	return(pvalues_glm_lrt(object, parm, H0, alternative, method, force, debuglevel))
}

pvalues_glm_lrt=function(object, parm, H0, alternative, method, force, debuglevel) {
	if (length(parm) != length(H0)) {stop("parm and H0 must have same length")}
	if (!is.numeric(parm)) {stop("parm must be numeric")}
	if (!is.numeric(H0)) {stop("H0 must be numeric")}
	pvalues = sapply(seq_along(parm), function (ip) {
		pvalue_glm_contrast_lrt(object,index2contrast(object,parm[ip]),H0=H0[ip],alternative=alternative,method=method,force=force,debuglevel=debuglevel)
	})
	names(pvalues)=names(coef(object))[parm]
	return(pvalues)
}

#' Hypothesis tests on contrasts
#'
#' This S3 generic function allows the computation of P-values associated to
#' hypothesis tests of contrasts (i.e. linear combinations) of fixed-effects in a model.
#' The default implementation computes Wald's P-values with any model as long as it consistently implements \code{\link{fixcoef}}, \code{\link{vcov_fixcoef}} and \code{\link{df_for_wald}}.
#' It is also specialized for GLMs and negative binomial models (see \code{\link[MASS:glm.nb]{MASS::glm.nb}}) with Wald's, LRT and Rao's P-values and may be specialized with other models.
#'
#' @param model a fitted statistical model such as a glm or a coxph.
#' @param contrast numeric vector of the same length as the number of coefficients in the model; it describes the contrast \code{sum(contrast*fixcoef(model))}.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param H0 numeric value; the value of the contrast under the null hypothesis.
#' @param method character string value; specification of the algorithm used (implementation dependent).
#' Suggested values are "Wald", "LRT", "Rao" and "exact" for, respectively, Wald's asymptotic normality and/or student test,
#' the Generalized Likelihood Ratio Test, Rao's score test and non-asymptotic exact tests. Other values may be allowed.
#' @param ... Additional parameters that may be used by some implementations.
#' @param debuglevel integer value; set to 0 (default) to disable warnings, 1 to enable warnings and 2 to enable warnings and notes.
#' @param force logical; if TRUE, force computation of P-values in case of convergence problems.
#' @return
#' A single numeric value (vector of length 1) equal to the one-sided (for alternative="less" or "greater") or two-sided P-value
#'
#' @details
#' Every implementation MUST support specification of the alternative hypothesis
#' (alternative argument) and null hypothesis (H0 argument).
#'
#' @examples
#' data(mtcars)
#' model1 = glm(family="gaussian", data=mtcars, hp ~ 0+factor(gear))
#' # do cars with 5 gears have more horse power (hp) than cars with 4 gears ?
#' p_value_contrast(model1, c(0,-1,1), alternative="greater")
#'
#' # now, we fit an equivalent model (same distribution and same predictions)
#' model2 = glm(family=gaussian(log), data=mtcars, hp ~ 0+factor(gear))
#'
#' # do cars with 5 gears have at least twice the horse power than cars with 4 gears ?
#'
#' # the following two tests are equivalent
#' p_value_contrast(model1, c(0,-1,0.5), alternative="greater", method="LRT", H0=0)
#' p_value_contrast(model2, c(0,-1,1), alternative="greater", method="LRT", H0=log(2))
#'
#' # the following two tests are close but not equivalent
#' p_value_contrast(model1, c(0,-1,0.5), alternative="greater", method="Wald", H0=0)
#' p_value_contrast(model2, c(0,-1,1), alternative="greater", method="Wald", H0=log(2))
#' @family Wald-related functions
#' @family Contrast functions
#' @export
p_value_contrast=function(model, contrast, alternative=c("two.sided","less","greater"), H0=0, method=NULL,  ...) {
	UseMethod("p_value_contrast")
}

assert_finite_numeric_vector=function(v, name) {
	if (!(is.numeric(v))) {
		stop(paste0(name, " must be a numeric vector"))
	}
	if (any(is.na(v))) {
		stop(paste0(name, " must have no NA values"))
	}
	if (any(!is.finite(v))) {
		stop(paste0(name, " must have only finite values"))
	}
}

assert_p_value_contrast_arguments=function(model,contrast,method,force,debuglevel,H0,alternative) {
	assert_bool(force, "force")
	assert_debug_level(debuglevel)
	assert_finite_numeric_vector(H0, "H0")
	assert_enumeration(alternative, "alternative", c("two.sided","less","greater"))
	if (length(H0) != 1) {stop("H0 must contain a single scalar")}
	if (length(contrast) != length(fixcoef(model))) {
		stop(paste0("contrast (length ", length(contrast), ") and fixcoef(model) (length ", length(fixcoef(model)), ") must have the same length"))
	}
	if (all(contrast == 0)) {
		stop("contrast must not be all-zero")
	}

}

#' @describeIn p_value_contrast It supports Wald (method="Wald"),
#' Generalized Likelihood Ratio Tests (method="LRT") and Rao's score tests (method="Rao").
#' It works for \code{\link[stats:glm]{stats::glm}} models and negative binomial models (\code{\link[MASS:glm.nb]{MASS::glm.nb}}) with method="LRT" and method="Wald".
#' @export
p_value_contrast.glm=function(model, contrast, alternative=c("two.sided","less","greater"), H0=0, method=c("LRT","Rao","Chisq","F","Wald", "wald"), ..., debuglevel=1, force=FALSE) {
	check_contrast(model, contrast)
	if (!inherits(model, "glm")) {
		stop("model must be glm")
	}
	if (length(setdiff(class(model), c("negbin", "glm", "lm")))>0) {
		# actually, it is a child class of glm... Most of them are actually incompatible.
		return(p_value_contrast.default(model, contrast, alternative=alternative, H0=H0, method=method, ..., debuglevel=debuglevel, force=force))
	}
	if (is.null(method)) {method="LRT"}
	assert_enumeration(method, "method", c("LRT","Rao","Chisq","F","Wald", "wald"))
	assert_p_value_contrast_arguments(model, contrast, method, force, debuglevel, H0, alternative)

	method = method[1]
	if (method %in% c("LRT", "Rao", "Chisq", "F")) {
		pvalue=pvalue_glm_contrast_lrt(model,contrast,H0=H0,alternative=alternative,method=method,force=force,debuglevel=debuglevel)
		names(pvalue)="pvalue"
	} else {
		pvalue = pvalue_glm_contrast_wald(model,contrast,H0=H0,alternative=alternative,force=force,debuglevel=debuglevel)["pvalue"]
	}
	structure(pvalue, method=method)
}

#' Generic function to get fixed effects of a model
#'
#' This is a generic S3 function that gets point estimates of fixed effects of a statistical model, implemented on a wide range of models and that can be extended to new models.
#'
#' @param model a fitted statistical model
#' @param ... argument unused by \code{\link{p_value_contrast.default}} but that may be useful to some specializations.
#'
#' @return
#' Simple numeric vector with one item for each fixed effect of the model.
#'
#' @details
#' It must return only estimates of fixed-effects of a model. Random effects are ignored.
#' The \code{\link[base:names]{names}} of the element of this vector must be consistent
#' with the \code{rownames} and \code{colnames}
#' of the variance-covariance matrix that \code{\link{vcov_fixcoef}} returns.
#' The \code{vcov_fixcoef} function, on the same model, must return a matrix
#' with the same number and names of rows and columns as the length of the vector returned by \code{fixcoef}.
#'
#' The functions \code{\link{vcov_fixcoef}} and \code{\link{fixcoef}} would be pointless if the behavior of
#' \code{\link[stats:vcov]{vcov}} and \code{\link[stats:coef]{coef}} were not inconsistent from package to package.
#'
#' \code{fixcoef} and \code{vcov_fixcoef}, together with \code{\link{df_for_wald}} are used by \code{\link{p_value_contrast.default}}
#'
#' @examples
#' data(mtcars)
#' fixcoef(lm(data=mtcars, hp ~ 1)) # get mean horse power of cars listed in mtcars
#' @family Wald-related functions
#'
#' @export
fixcoef=function(model, ...) {
	UseMethod("fixcoef")
}

#' @describeIn fixcoef implementation for \code{\link[lme4:lmer]{lme4::lmer}}
#' @export
fixcoef.lmerMod=function(model, ...) {
	requireNamespace("lme4")
	#get("fixef", envir=loadNamespace("lme4"), mode="function")(model)
	lme4::fixef(model)
}

#' @describeIn fixcoef implementation for \code{\link[lme4:glmer]{lme4::glmer}}
#' @export
fixcoef.glmerMod=fixcoef.lmerMod

#' @describeIn fixcoef implementation for \code{\link[lmerTest:lmer]{lmerTest::lmer}}
#' @export
fixcoef.lmerModLmerTest=fixcoef.lmerMod

#' @describeIn fixcoef implementation for \code{\link[nlme:lme]{nlme::lme}}
#' @export
fixcoef.lme=function(model, ...) {
	requireNamespace("nlme")
	nlme::fixef(model)
}

#' @describeIn fixcoef implementation for \code{\link[nnet:multinom]{nnet::multinom}}
#' @export
fixcoef.multinom=function(model, ...) {
	if (is.vector(coef(model))) {return(coef(model))}
	x = t(coef(model))
	val = matrix(nrow=nrow(x), ncol=ncol(x), colnames(x), byrow=T)
	nm  = matrix(nrow=nrow(x), ncol=ncol(x), rownames(x), byrow=F)
	x = as.vector(x)
	names(x) = paste0(as.vector(val), ":", as.vector(nm))
	x
}

#' @describeIn fixcoef implementation for multiple responses linear models generated by \code{\link[stats:lm]{stats::lm}} when the response is a matrix.
#' It transforms the matrix to a vector, consistent with \code{\link[stats:vcov]{stats::vcov}}.
#' @export
fixcoef.mlm=function(model, ...) {
	x = coef(model)
	val = matrix(nrow=nrow(x), ncol=ncol(x), colnames(x), byrow=T)
	nm  = matrix(nrow=nrow(x), ncol=ncol(x), rownames(x), byrow=F)
	x = as.vector(x)
	names(x) = paste0(as.vector(val), ":", as.vector(nm))
	x
}

#' @describeIn fixcoef default implementation, simply calls coef(model).
#' @export
fixcoef.default=function(model, ...) {coef(model)}


#' Gets the variance-covariance matrix of fixed effects of a fitted model
#'
#' This is a generic S3 function that gets the variance-covariance matrix of fixed effects of a statistical model, implemented on a wide range of models and that can be extended to new models.
#'
#' @param model a fitted statistical model
#' @param ... argument unused by \code{\link{p_value_contrast.default}} but that may be useful to some specializations.
#'
#' @details
#' It must return variance-covariance for fixed effects of a model, not random effects nor scale parameters.
#' The \code{rownames} and \code{colnames} of the returned matrix
#' must be consistent with \code{\link[base:names]{names}} of \code{\link{fixcoef}(object)}.
#'
#' The functions \code{\link{vcov_fixcoef}} and \code{\link{fixcoef}} would be pointless if the behavior of
#' \code{\link[stats:vcov]{vcov}} and \code{\link[stats:coef]{coef}} were not inconsistent from package to package.
#'
#' \code{fixcoef} and \code{vcov_fixcoef}, together with \code{\link{df_for_wald}} are used by \code{\link{p_value_contrast.default}}
#'
#' @examples
#' data(mtcars)
#' mod = lm(data=mtcars, hp ~ cyl+wt)
#' est = fixcoef(mod) # get estimates
#' SE = sqrt(diag(vcov_fixcoef(mod))) # get standard errors of estimates
#' z  = est/SE # get z-score of estimates
#' df = df_for_wald(mod) # degrees of freedom
#' pvalues = 2*pt(-abs(z), df=df) # get two-sided P-values
#' @family Wald-related functions
#' @export
vcov_fixcoef=function(model, ...) {
	UseMethod("vcov_fixcoef")
}

#' @describeIn vcov_fixcoef default implementation, simple proxy of \code{\link[stats:vcov]{vcov}(model)}
#' @export
vcov_fixcoef.default=function(model, ...) {vcov(model, ...)}

#' @describeIn vcov_fixcoef implementation for survreg, removing the extra column for Scale
#' @export
vcov_fixcoef.survreg=function(model, ...) {
	vc = vcov(model)
	xc = coef(model)
	vc = vc[seq_along(xc), seq_along(xc)]
	rownames(vc)=names(xc)
	colnames(vc)=names(xc)
	return(vc)
}

# this function is designed to work with all models supporting fixcoef() and vcov()
#
pvalue_glm_contrast_wald=function(model, contrast, H0=0, alternative = "two.sided", force=FALSE, debuglevel=1, conf.level=0.95) {
	if (!force) {
		smod = add_warnings(model, list())
		if (!is.null(smod$problem_of_convergence)) {
			warn(debuglevel,paste0("Wald's P-value of contrast not computed on model that did not converge (convergence problem=", smod$problem_of_convergence, ")\n"))
			return(NA)
		}
	}
	xcoef = fixcoef(model)
	coef_ok = !is.na(xcoef)
	if (any(contrast[!coef_ok] != 0)) {
		note(debuglevel,"contrast is based on non-estimable coefficients\n")
		return (NA)
	}
	vc = vcov_fixcoef(model)
	if (is.null(rownames(vc)) | is.null(colnames(vc))) {
		if (nrow(vc) != ncol(vc)) {
			stop("the variance-covariance matrix is not square")
		}
		if (nrow(vc) == length(xcoef)) {
			rownames(vc) = names(xcoef)
			colnames(vc) = names(xcoef)
		} else {
			stop(paste0("the variance-covariance matrix size (",nrow(vc), ") does not match the number of fix coefficients (", length(xcoef), ")"))
		}
	}
	vc = vc[names(xcoef),names(xcoef),drop=FALSE] # make polr work
	contrast = contrast[coef_ok]
	vc = vc[coef_ok,coef_ok,drop=FALSE]
	xcoef = xcoef[coef_ok]

	SE = sqrt(as.vector(t(contrast) %*% vc %*% contrast))
	estimate = sum(xcoef * contrast) - H0
	z = estimate/SE
	dfw = df_for_wald(model)
	
	alternative = alternative[1]
	
	if (alternative == "two.sided") {
		ci = estimate + qt(c((1-conf.level)/2, 1-(1-conf.level)/2), df=dfw)*SE
	} else if (alternative == "less") {
		ci = estimate + qt(c(0, conf.level), df=dfw)*SE
	} else if (alternative == "greater") {
		ci = estimate + qt(c(1-conf.level, 1), df=dfw)*SE
	} else {
		stop(paste0("unknown alternative (H1): ", alternative))
	}
	pvalue = 2*pt(-abs(z), df=dfw)
	pvalue = twosided2pvalue(pvalue,estimate,alternative)
	names(pvalue)=NULL;names(z)=NULL;names(ci)=NULL;
	return(c(pvalue=pvalue,z=z, lower=ci[1], upper=ci[2], SE=SE))
}

#' @describeIn p_value_contrast
#' Supports Wald's test on a wide range of models, including \code{\link[stats:lm]{lm}}, \code{\link[stats:lm]{mlm}},
#' \code{\link[stats:glm]{stats::glm}}, \code{\link[MASS:glm.nb]{negbin}}, \code{\link[MASS:polr]{MASS::polr}},
#' \code{\link[MASS:rlm]{MASS::rlm}} (with normality assumptions, defeating the purpose of rlm), \code{\link[nlme:lme]{nlme::lme}}, 
#' \code{\link[nlme:gls]{nlme::gls}}, \code{\link[lme4:lmer]{lme4::lmer}}, \code{\link[lme4:glmer]{lme4::glmer}}, 
#' \code{\link[mgcv:gam]{mgcv::gam}}, \code{\link[gam:gam]{gam::gam}}, \code{\link[survival:coxph]{survival::coxph}}, 
#' \code{\link[survival:survreg]{survival::survreg}}, 
#' \code{\link[nnet:multinom]{nnet::multinom}}, \code{\link[stats:nls]{stats::nls}}.
#' 
#' It can be easily extended by implementing three generic functions:
#' \code{\link[glmglrt:fixcoef]{fixcoef}}, \code{\link[glmglrt:vcov_fixcoef]{vcov_fixcoef}} and \code{\link[glmglrt:df_for_wald]{df_for_wald}}.
#' If the implementations of \code{\link[stats:coef]{coef}}, \code{\link[stats:vcov]{vcov}} and \code{\link[stats:df.residual]{df.residual}} are consistent,
#' you do not have to implement \code{fixcoef}, \code{vcov_fixcoef} and \code{df_for_wald}.
#'
#' @export
p_value_contrast.default=function(model, contrast, alternative=c("two.sided","less","greater"), H0=0, method="Wald", ..., debuglevel=0, force=FALSE) {
	# ok with lm()
	check_contrast(model, contrast)
	
	if (is.null(method)) {method = "Wald"}
	assert_enumeration(method, "method", c("Wald","wald", "SlowWald"))
	assert_p_value_contrast_arguments(model, contrast, method, force, debuglevel, H0, alternative)

	structure(pvalue_glm_contrast_wald(model, contrast, H0=H0, alternative=alternative, force=TRUE, debuglevel=debuglevel)["pvalue"], method=method)
}

recode_alternative=function(alternative) {
	if (!is.vector(alternative) || !is.character(alternative)) {
		stop("alternative must be a character vector")
	}
	if (length(alternative)==0) {
		stop("an alternative must be specified")
	}

	alts = c("two.sided","less","greater")
	ok = grepl(paste0("^", alternative[1]), alts)
	if (!any(ok)) {
		stop("alternative must be \"two.sided\", \"less\" or \"greater\" or a shortened version")
	}
	if (sum(ok)>1) {
		stop("ambiguous alternative")
	}
	alts[ok]
}

#' Computing p-values of hypothesis tests on coefficients of Generalized Linear Models and other
#'
#' This S3 method is a specialization of \code{\link[parameters:p_value]{parameters::p_value}}
#' for \code{\link[stats:glm]{stats::glm}} models.
#' By default, it computes Wald's P-values that are known to be more biased than LRT P-values,
#' but the behavior can be overriden by the method="LRT" argument.
#' This is for compatibility with the default method of \code{\link[parameters:p_value]{parameters::p_value}}.
#'
#' @param model glm object; as obtained by calling \code{\link[stats:glm]{stats::glm}} or \code{\link[MASS:glm.nb]{MASS::glm.nb}}.
#' @param method character value; may either be "LRT" (synonym "Chisq"), "Rao", "wald" (default value, synonym "Wald" and "F").
#' @param parm integer or character vector or NULL; specify coefficients to test, by name or indexes.
#' the default parm=NULL outputs all coefficients.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param H0 numeric vector of length 1 or of the same length as parm; the value of the coefficient under the null hypothesis. Zero by default.
#' @param debuglevel integer value; set to 0 (default) to disable warnings, 1 to enable warnings and 2 to enable warnings and notes.
#' @param force logical; if TRUE, force computation of P-values in case of convergence problems.
#' @param ... Ignored arguments. Allows compatibility with the generic \code{\link[parameters:p_value]{parameters::p_value}}.
#' @return a data.frame with two columns; the first column, Parameter represents the name of the coefficient
#' and p (second column) represents the P-value of the hypothesis test against H0
#' @examples
#' require("parameters")
#' mod = glm(family="poisson", c(2,30) ~ c(0,1), offset=log(c(8,30)))
#' # Wald's tests (biased)
#' p_value(mod)
#' # Rao score tests (biased)
#' p_value(mod, method="Rao")
#' # LRT tests (less biased)
#' p_value(mod, method="LRT")
#'
#' # only test slope (faster since only one test is performed)
#' p_value(mod, method="LRT", parm=2)
#' # is slope greater than log(2) ?
#' p_value(mod, method="LRT", parm=2, H0=log(2), alternative="greater")
#' @export
p_value.glm=function(model, method = NULL, parm=NULL, alternative=c("two.sided","less","greater"), H0=0, debuglevel=1, force=FALSE, ...) {
	if (!inherits(model, "glm")) {stop("model must be glm")}
	if (is.null(method)) {method = "Wald"}

	if (!is.vector(method) | !is.character(method) | length(method) != 1) {
		stop("method must be a single character string")
	}
	method = translate_methods(method)
	alternative=recode_alternative(alternative)

	pvalues = pvalues_glm(model, parm=parm, alternative=alternative, H0=H0, method=method, debuglevel=debuglevel, force=force)
	df = data.frame(Parameter = names(pvalues), p = pvalues, stringsAsFactors=FALSE)
	rownames(df)=NULL
	return(df)
}

translate_methods=function(methods) {
	sapply(methods, function(method) {assert_enumeration(method, "method", c("Wald", "wald", "Chisq", "LRT", "Rao", "F"))})
	methods[methods == "Chisq"]="LRT"
	methods[methods == "wald"]="Wald"
	methods[methods == "F"]="Wald"
	return(methods)
}
#' Summarizes a glm, adding a column of GLRT or Rao score P-values
#'
#' \code{summarylr} is an improved summary function for standard glm (stats package) adding LRT or Rao score P-values
#'
#' This function works the same as the standard \code{\link[stats]{summary.glm}} function but provides additionnal parameters
#' The core parameter \code{method="LRT"} makes \code{summarylr} adds a column \code{LRT P-value}
#' to the output. This P-value is computed by repeatdly fitting the model dropping
#' one coefficient at a time and using the \code{\link[stats:anova.glm]{anova.glm(test="Chisq")}} function to perform
#' generalized likelihood ratio test by approximation of the deviance difference
#' to a chi-square distribution. This provides P-values less biased than the standard Wald P-values
#' that \code{summary} provides. Moreover, this LRT method is consistent with the profile likelihood
#' confidence intervals that \code{\link[MASS:confint]{confint.glm}} provides.
#' The option \code{method="Rao"} generates Rao's score P-values. \code{method="Chisq"} is synonymous to \code{method="LRT"}.
#' For exhaustivity, the option \code{method="Wald"} (synonym "wald", "F") generates Wald's P-values.
#' Several methods can be used, e.g. \code{method=c("LRT","Rao")} computes both LRT and Rao P-values. New methods may be added in the future.
#'
#' Extra parameters are passed-through to the \code{\link[stats]{summary.glm}} function.
#'
#' @param object glm object; as obtained by calling \code{\link[stats:glm]{stats::glm}} or \code{\link[MASS:glm.nb]{MASS::glm.nb}}.
#' @param dispersion the dispersion parameter for the family used. Either a single numerical value or NULL (the default), when it is inferred from object (see \code{\link[stats:summary.glm]{stats::summary.glm}}).
#' @param correlation logical; if TRUE, the correlation matrix of the estimated parameters is returned and printed (see \code{\link[stats:summary.glm]{stats::summary.glm}}).
#' @param symbolic.cor logical; if TRUE, print the correlations in a symbolic form (see \code{\link[stats:symnum]{symnum}}) rather than as numbers (see \code{\link[stats:summary.glm]{stats::summary.glm}}).
#' @param ... Additional arguments to be passed to \code{\link[stats:summary.glm]{stats::summary.glm}}
#' @param force logical; if TRUE, force computation of P-values in case of convergence problems.
#' @param debuglevel integer value; set to 0 (default) to disable warnings, 1 to enable warnings and 2 to enable warnings and notes.
#' @param keep.wald logical; if TRUE, the standard Wald's P-values are kept in the output of \code{\link{print.summary.glmglrt}}. Even if keep.wald=FALSE, the standard wald P-values are not erased from the \code{summary.glmglrt} object. They are only hidden by \code{\link{print.summary.glmglrt}}.
#' @param method NULL or character vector of length 0, 1 or 2; may be code{"LRT"} or \code{"Rao"} or \code{c("LRT", "Rao")} to compute specified P-values.
#' You can set \code{method=NULL} to compute no additional P-values.
#' @return
#' It returns a summary object of type \code{summary.glmglrt} that gets pretty printed by \code{link[glmglrt:print.summary.glmglrt]{print.summary.glmglrt}}
#' The return value is an S3 object compatible with \code{\link[stats:summary.glm]{stats::summary.glm}} but with an additional field \code{$extra} field having sub-fields.
#' \code{$extra$pvalues} is a numeric matrix with columns "LRT P-value" and/or "Rao P-value", containing the relevant P-values. As new columns may be added in future, you should rely on column names rather than column indexes. Only P-values of methods requested in the \code{method} parameter are stored in this matrix.
#' \code{$extra$debuglevel} is equal to the \code{debuglevel} passed to \code{summarylr}.
#' \code{$extra$keep.wald} is equal to the \code{keep.wald} passed to \code{summarylr}.
#' In case of convergence problems, the field \code{$extra$problem_of_convergence} will be added. It will be a character string with the value \code{"general"} (because model$converged = FALSE), \code{"all"} (because all coefficients have huge variances) or \code{"specific"} (because at least one coefficient has a huge variance). Other problem strings may be added in the future.
#' If \link[stats:glm]{weights} are specified in a way that make P-values invalid, the field \code{$extra$problem_weights} will be added as a character string describing the problem. Actually, the only known problem is \code{"non-constant"}.
#' @examples
#' summarylr(glm(family="binomial", cbind(5,3)~1))
#' data(mtcars)
#' # do not properly converge (warnings)
#' mtcars$outcome = mtcars$disp > median(mtcars$disp)
#' mod=glm(family=binomial(log), data=mtcars,outcome ~ 0+qsec+wt,start=c(-0.1,0.3))
#' summarylr(mod) # warns that P-values are not computed because model did not converge
#' summarylr(mod, force=TRUE) # compute P-values anyway !
#' # also works with negative binomial models
#' summarylr(MASS::glm.nb(data=mtcars, I(cyl*gear) ~ 1+wt,link="sqrt"),test="LRT")
#' @family Extended GLM summary functions
#' @export
summarylr=function(object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, ..., force = FALSE, debuglevel = level_warning, method = "LRT", keep.wald=FALSE) {
	if (!inherits(object, "glm")) {stop("object must be a glm")}
	assert_bool(force, "force")
	assert_debug_level(debuglevel)
	if (!is.null(method)) {
	if (!is.vector(method) || !is.character(method) || !all(method %in% c("LRT","Rao","Chisq","Wald","wald","F"))) {
		stop("method must be NULL, LRT, Rao, Wald or a vector combining one or more of them")
	}
	}
	method = translate_methods(method)
	method = sort(unique(method))

	if (inherits(object, "negbin")) {
		mass_summary_negbin = getS3method("summary","negbin",envir=loadNamespace("MASS"))
		if (is.null(dispersion)) {dispersion=1}
		smod = mass_summary_negbin(object, dispersion=dispersion, correlation=correlation, symbolic.cor=symbolic.cor, ...)
	} else {
		smod = stats::summary.glm (object, dispersion=dispersion, correlation=correlation, symbolic.cor=symbolic.cor, ...)
	}
	class(smod)=c("summary.glmglrt", class(smod))
	smod$extra = list() # where we add all the new parameters
	smod$extra$debuglevel = debuglevel
	smod$extra$keep.wald = keep.wald

	pvalues = do.call(cbind, lapply(method, function(meth) {
		pvalues = pvalues_glm(object, parm=which(!is.na(coef(object))), method=meth, force=force, debuglevel=debuglevel)
		pvalues = as.matrix(pvalues)
		colnames(pvalues) = paste0(meth, " P-value")
		#smod$coefficients = cbind(smod$coefficients, pvalues)
		return(pvalues)
	}))

	smod$extra$pvalues = pvalues


	smod = add_warnings(object, smod)

	ini_weights =get_ini_weights(object)
	if (!all(ini_weights == ini_weights[1]) & family(object)$family != "binomial") {
		smod$extra$problem_weights = "non-constant"
	}
	return(smod)
}

#' Overrides the Generalized Linear Models summary methods
#'
#' This function overrides the \code{\link[stats:summary.glm]{summary.glm}}
#' and \code{\link[stats:summary.glm]{summary.negbin}} S3 methods
#' by the \code{\link{summarylr}} function in the calling environment.
#'
#' @details
#' Although some minor compatibility issues may exist when calling this function in the global environment,
#' most scripts should work with it. Indeed \code{\link{summarylr}} behaves like \code{\link[stats:summary.glm]{summary.glm}}
#' but adds a $extra field containing P-value info. The first letter of the field name ('e') is unique, avoiding problems
#' with scripts that access fields with short names (e.g. model$x for model$xlevels).
#' @examples
#' model = glm(family="binomial", cbind(50,30) ~ 1)
#' override_summary()
#' summary(model) # Additional 'LRT P-value' column
#' @family Extended GLM summary functions
#' @export
override_summary = function() {
	assign("summary.glm", summarylr, envir = parent.frame())
	assign("summary.negbin", summarylr, envir = parent.frame())
}

is_relative_family=function(fam) {
	fam$link %in% c("logit", "log") & fam$family %in% c("poisson", "binomial")
}

#' Prints the summary generated by \code{\link{summarylr}}
#'
#' This function prints a \code{summary.glmglrt} object generated by \code{\link{summarylr}}.
#' It works like the standard \code{\link[stats:summary.glm]{summary.glm}} function but additionnally
#' displays columns showing Rao or LRT P-values.
#'
#' @param x a \code{summary.glmglrt} object generated by \code{\link{summarylr}}.
#' @param ... additional arguments passed to \code{\link[stats:summary.glm]{stats::print.summary.glm}} then \code{\link[stats:printCoefmat]{printCoefmat}}. The most useful ones are \code{digits} and \code{signif.stars}.
#' @param has.Pvalue logical value; passed to \code{\link[stats:printCoefmat]{printCoefmat}}; if TRUE, the P-value column is formatted by \code{\link[base:format.pval]{format.pval}}.
#' @param tst.ind integer vector of length>=0; passed to \code{\link[stats:printCoefmat]{printCoefmat}}; it changes the format of these columns, assuming they are statistics columns.
#' @param debuglevel NULL or integer value; set to NULL to use the debuglevel argument that was specified in  \code{\link{summarylr}}, 0 (default) to disable warnings, 1 to enable warnings and 2 to enable warnings and notes.
#' @param keep.wald NULL or logical; set to NULL to use the keep.wald argument that was specified in \code{\link{summarylr}}. If TRUE, the standard Wald's P-values are displayed. If FALSE, the standard Wald's P-values are hidden.
#'
#' @examples
#' model = glm(family="binomial", cbind(50,30) ~ 1)
#' print(summarylr(model),signif.stars=FALSE,digits=10)
#'
#' @family Extended GLM summary functions
#'
#' @export
print.summary.glmglrt=function (x, ..., has.Pvalue=TRUE, tst.ind=3, debuglevel=NULL, keep.wald=NULL) {
	if (!inherits(x, "summary.glmglrt")) {
		stop("summary.glmglrt object expected")
	}
	if (is.null(x$extra)) {
		stop("$extra info of summary.glmglrt object required")
	}
	if (is.null(keep.wald)) {keep.wald=x$extra$keep.wald}
	if (is.null(keep.wald)) {keep.wald=FALSE}
	if (!keep.wald) {
		bad = grepl("Pr\\(>\\|[zt]\\|\\)", colnames(x$coefficients))
		if (sum(bad)==1) { # remove Wald test
			x$coefficients = x$coefficients[,!bad,drop=FALSE]
		}
	}
	x$coefficients = cbind(x$coefficients, x$extra$pvalues)
	if (any(x$aliased)) {
		m = matrix(nrow=length(x$aliased), ncol=ncol(x$coefficients), NA)
		rownames(m)=names(x$aliased)
		colnames(m)=colnames(x$coefficients)
		m[!x$aliased,] = x$coefficients
		x$coefficients = m
		x$aliased[TRUE]=FALSE
	}
	class(x) = setdiff(class(x), "summary.glmglrt")
	out = print(x, has.Pvalue=has.Pvalue, tst.ind=tst.ind, ...)

	if (is.null(debuglevel)) {debuglevel = x$extra$debuglevel}
	if (is.null(debuglevel)) {debuglevel = 1}

	if (!is.null(x$extra$problem_of_convergence)) {
		problem = x$extra$problem_of_convergence
		if (problem %in% c("general", "all")) {
			warn(debuglevel,"Model did not converge at all. Point estimates may be grossly wrong. Standard errors and P-values are wrong.")
		} else if (problem == "specific") {
			warn(debuglevel,"Some coefficients are not estimable as shown by huge standard errors. Estimates of these coefficients are wrong. Other coefficients may be affected.")
		} else {
			warn(debuglevel,"convergence problem: ", problem)
		}
	}
	if (!is.null(x$extra$problem_weights)) {
		problem = x$extra$problem_weights
		if (problem == "non-constant") {
			note(debuglevel,"Weights are not constant.Unless weights are the inverse of the variance of observations, P-values are biased.")
		} else {
			note(debuglevel,"problem with weights: ", problem)
		}
	}
	invisible(out)
}

#' Point estimates of contrasts
#'
#' This S3 generic function allows the computation of point estimates of contrasts
#' (i.e. linear combinations) of fixed-effects in many models
#' The default implementation computes Wald's confidence intervals with any model as long as it implements \code{\link{fixcoef}}, returning a vector of fixed effects.
#'
#' @param model a fitted statistical model such as a glm or a coxph.
#' @param contrast numeric vector of the same length as the number of coefficients in the model; it describes the contrast \code{sum(contrast*fixcoef(model))}.
#' @param method character string value; specification of the algorithm used (implementation dependent). NULL must be accepted.
#'   Suggested values are "ML" for maximum-likelihood, "REML" for restricted maximum-likelihood and "OLS" for ordinary least squares.
#' @param ... Additional parameters that may be used by some implementations.
#' @return
#' A single numeric value (vector of length 1) equal to the point estimate of the contrast, with the name "pvalue".
#'
#' @details
#' This function should consistent with \code{\link{confint_contrast}} and \code{\link{p_value_contrast}} as they are designed to be used together.
#' If a null hypothesis (H0) is specified, it MUST be ignored by \code{estimate_contrast}.
#' If you want to make it consistent with p_value_contrast you may substract H0 from the output of \code{estimate_contrast} and \code{confint_contrast}.
#'
#' @family Contrast functions
#'
#' @examples
#' data(mtcars)
#' model1 = glm(family="gaussian", data=mtcars, hp ~ 0+factor(gear))
#' # do cars with 5 gears have more horse power (hp) than cars with 4 gears ?
#' estimate_contrast(model1, c(0,-1,1))
#'
#' # now, we fit an equivalent model (same distribution and same predictions)
#' model2 = glm(family=gaussian(log), data=mtcars, hp ~ 0+factor(gear))
#'
#' # do cars with 5 gears have at least twice the horse power than cars with 4 gears ?
#'
#' estimate_contrast(model1, c(0,-1,0.5))
#'
#' @export
estimate_contrast=function(model, contrast, method=NULL, ...) {
	UseMethod("estimate_contrast", model)
}

#' Confidence interval estimation of contrasts
#'
#' This S3 generic function allows the computation of confidence intervals of contrasts
#' (i.e. linear combinations) of fixed-effects in many models.
#' The default implementation computes Wald's confidence intervals with any model as long as it consistently implements \code{\link{fixcoef}}, \code{\link{vcov_fixcoef}} and \code{\link{df_for_wald}}.
#' It is also specialized for GLMs with Wald's, LRT and Rao's confidence intervals and may be specialized with other models.
#'
#' @param model a fitted statistical model such as a glm or a coxph. 
#' @param contrast numeric vector of the same length as the number of coefficients in the model; it describes the contrast \code{sum(contrast*fixcoef(model))}.
#' @param method character string value; specification of the algorithm used (implementation dependent). NULL must be accepted.
#'   Suggested values are "LRT" for inverted likelihood ratio test, "Rao" for inverted Rao's score test, "Wald" for inverted Wald's test.
#' @param level numeric value between 0 and 1; nominal confidence level.
#' @param alternative character value; either "two.sided", "less" or "greater", specifying a two-sided or one-sided confidence interval.
#' @param ... Additional parameters that may be used by some implementations.
#' @param debuglevel integer value; set to 0 (default) to disable warnings, 1 to enable warnings and 2 to enable warnings and notes.
#' @param force logical; if TRUE, force computation of P-values in case of convergence problems.
#' @return
#' A vector of length 2. The first value MUST be named "lower" and be the lower bound of the confidence interval.
#' The second value MUST be named "upper" and be the upper bound of the confidence interval.
#'
#' @details
#' This function should consistent with \code{\link{estimate_contrast}} and \code{\link{p_value_contrast}} as they are designed to be used together.
#' If a null hypothesis (H0) is specified, it MUST be ignored by \code{confint_contrast} as in \code{estimate_contrast}.
#' If you want to make it consistent with \code{p_value_contrast} you may subtract H0 from the output of \code{estimate_contrast} and \code{confint_contrast}.
#' 
#' When alternative is "less" or  "greater", one-sided confidence intervals are generated.
#'
#' @family Contrast functions
#'
#' @examples
#' data(mtcars)
#' model1 = glm(family="gaussian", data=mtcars, hp ~ 0+factor(gear))
#' # do cars with 5 gears have more horse power (hp) than cars with 4 gears ?
#' confint_contrast(model1, c(0,-1,1))
#'
#' # now, we fit an equivalent model (same distribution and same predictions)
#' model2 = glm(family=gaussian(log), data=mtcars, hp ~ 0+factor(gear))
#'
#' # do cars with 5 gears have at least twice the horse power than cars with 4 gears ?
#'
#' confint_contrast(model2, c(0,-1,0.5))
#'
#' @export
confint_contrast=function(model, contrast, method=NULL, level=0.95, alternative=c("two.sided", "less", "greater"), ...) {
	UseMethod("confint_contrast", model)
}

check_contrast=function(model, contrast) {
	if (!is.vector(contrast) || !is.numeric(contrast)) {stop("contrast must be a numeric vector")}
	coefs = fixcoef(model)
	if (!is.vector(coefs   ) || !is.numeric(coefs   )) {stop("fixcoef(model) must be a numeric vector")}
	if (length(coefs) != length(contrast)) {
		stop("length of contrast must be the same as fixcoef(model)")
	}
	if (any(contrast[is.na(coefs)]!=0)) {
		stop("contrast should be zero on NA coefficients")
	}
}

#' @describeIn estimate_contrast Compute contrasts of fixed-effects in any model implementing \code{\link{fixcoef}}.
#' It basically computes \code{sum(fixcoef(model) * contrast)}.
#' @export
estimate_contrast.default=function(model, contrast, method=NULL, ...) {
	check_contrast(model, contrast)
	return (sum(fixcoef(model) * contrast))
}

logit=function(p) {log(p/(1-p))}

#' Newton-Raphson algorithm when parameters may not be estimable outside of their parameter space
#'
#' This is used by \code{\link{confint_contrast.default}} to find the lower and upper boundaries of the confidence interval so that they have a P-value equal to 1-level.
#' This makes it possible to compute contrast on log-binomial models when solutions are close to the boundaries of the parameter space 
#' and Newton-Raphson may compute some intermediate values outside of the valid parameter space, leading to P-values equal to NA.
#' 
#' It assumes that \code{rootfun} is NA above the upper boundary of the parameter space and below the lower boundary of the parameter space.
#' The parameter space must not have "holes" (i.e. valid, then invalid, then valid again).
#' Invalid values are described by NA. The actual interval of valid values are automatically found by the algorithm, so you do 
#' not have to explicitly specify the minimum and maximum values of the parameter space.
#'
#' @param rootfun a function taking a single numeric value. The algorithm searches for a root (zero) of this function.
#' @param x a single numeric value; starting value, not too far from the root of rootfun.
#' @param xabstol a single numeric value; when two consecutive iterations of the algorithm lead to x solutions with a difference larger than xabstol, 
#' the algorithm has not yet converged and continues to run. Set to Inf, if you want to only want to rely on yabstol.
#' @param derivdelta a single numeric value; the numeric delta used for numeric derivation of rootfun (assessed at x-derivdelta and x+derivdelta)
#' @param yabstol a single numeric value; when the algorithm leads to a y=rootfun(x) larger (in absolute value) than yabstol, the algorithm has not yet converged 
#' and continues to run. Set to Inf, if you want to only want to rely on xabstol.
#' @param niter a single integer value; the maximum number of iterations before considering that the algorithm failed to converge.
#' @param extraiter a single integer value; the number of extra iterations performed once convergence has been found.
#'   This is useful to make sure that convergence is good on platforms having high floating point precision allowing to use, at the same time,
#'   a weak convergence tolerance to make sure it converges on platforms having low floating point precision.
#'
#' @return
#' a list object with the following fields:
#' \describe{
#' \item{\code{root}}{the x value so that rootfun(x) is as close as possible to 0}
#' \item{\code{f.root}}{rootfun(root)}
#' \item{\code{yabstol}}{the parameter of the same name}
#' \item{\code{iter}}{the number of iterations of the Newton-Raphson algorithm performed before reaching convergence}
#' \item{\code{estim.prec}}{the latest move (on the x variable) done before convergence}
#' \item{\code{prevx}}{the previous x value, in the algorithm, just before x=root was reached}
#' \item{\code{prevy}}{rootfun(prevx)}
#' \item{\code{prevder}}{numeric derivative of rootfun at prevx}
#' }
#'
ps_newtonRaphson=function(rootfun, x, xabstol=1e-5, derivdelta=xabstol, yabstol=1e-5, niter=100, extraiter=0) {
	deriv=function(x) {
		(rootfun(x+derivdelta) - rootfun(x-derivdelta))/(2*derivdelta)
	}
	
	if (!is.finite(derivdelta)) {
		stop("derivdelta must be finite")
	}
	if (derivdelta <= 0) {
		stop("derivdelta must be positive")
	}
	if (!is.finite(x)) {
		stop("ps_newtonRaphson: starting value must be finite")
	}
	y = rootfun(x)
	if (!is.finite(y)) {
		stop("ps_newtonRaphson: starting value outside of parameter space")
	}
	terminate=FALSE
	xmax = Inf
	xmin = -Inf
	i=1
	success=FALSE
	while (i < niter | success & extraiter>0) {
		# at any time : x is assessable (inside boundary space) but xnext may not be
		if (success) {extraiter = extraiter - 1}
		
		der = deriv(x)
		if (!is.finite(der)) {
			stop("ps_newtonRaphson: derivative outside of parameter space")
		}
		if (der == 0) {
			stop("ps_newtonRaphson: derivative is zero")
		}
		dr = deriv(x)
		xnext = x - y/dr
		firstxnext = xnext
		

		# fail if actual solution seems to be outside of the parameter space
		if ((x == xmax & xnext > xmax) | (x == xmin & xnext < xmin)) {
			stop("ps_newtonRaphson: on a parameter space boundary and trying to go further away\n(i.e. either solution is outside of parameter space or starting value is too far from solution)")
		}
		
		# if outside the parameter space, move it to the closest boundary
		if (xnext > xmax) {xnext=xmax}
		if (xnext < xmin) {xnext=xmin}

		ynext = rootfun(xnext) # may or may not be within the parameter space
		
		if (!is.finite(ynext)) {
			# We try to find the boundary of the parameter space
			rt = uniroot(interval=sort(c(x, xnext)), f=function(x) {
				is.na(rootfun(x))-0.5
			}, tol=derivdelta)
			if (abs(rt$estim.prec)>2*derivdelta) {
				stop("ps_newtonRaphson: failed to find the boundary of the parameter space with uniroot")
			}
			xnext = rt$root
			xnext = xnext - sign(xnext - x)*abs(derivdelta*100) # permits to use deriv() on xnext
			if (xnext > x) {xmax=xnext}
			else {xmin=xnext}
			
			ynext = rootfun(xnext)
			if (!is.finite(ynext)) {
				stop("ps_newtonRaphson: failed to find the boundary of the parameter space")
			}
		}
		
		if (!is.finite(xnext) | !is.finite(ynext)) {
			stop("ps_newtonRaphson: internal error. At this point, xnext and ynext should both be finite.")
		}
		if (abs(x - firstxnext) < xabstol & abs(ynext) < yabstol) { # check convergence
			success = TRUE
		}
		if (success & extraiter <= 0) {
			if (extraiter < 0) {xnext=x;ynext=y;}
			out = list(
				root=xnext,
				f.root=ynext,
				yabstol=yabstol,
				iter = i,
				estim.prec = abs(x - firstxnext),
				prevx=x,
				prevy=y,
				prevder=dr
			)
			return(out)
		}
		x = xnext # finite
		y = ynext # finite
	}
	i = i + 1
	stop(paste0("ps_newtonRaphson: failed to converge at tolerance=", xabstol, " in ", niter, " iterations"))
}

real_glm=function(model) {
	inherits(model, "glm") && length(setdiff(class(model), c("glm", "lm", "negbin")))==0
}

#' @describeIn confint_contrast Default implementation
#' Supports Wald's test on a wide range of models, including \code{\link[stats:lm]{lm}}, \code{\link[stats:lm]{mlm}},
#' \code{\link[stats:glm]{stats::glm}}, \code{\link[MASS:glm.nb]{negbin}}, \code{\link[MASS:polr]{MASS::polr}},
#' \code{\link[MASS:rlm]{MASS::rlm}} (with normality assumptions, defeating the purpose of rlm), \code{\link[nlme:lme]{nlme::lme}}, 
#' \code{\link[nlme:gls]{nlme::gls}}, \code{\link[lme4:lmer]{lme4::lmer}}, \code{\link[lme4:glmer]{lme4::glmer}}, 
#' \code{\link[mgcv:gam]{mgcv::gam}}, \code{\link[gam:gam]{gam::gam}}, \code{\link[survival:coxph]{survival::coxph}}, 
#' \code{\link[survival:survreg]{survival::survreg}}, 
#' \code{\link[nnet:multinom]{nnet::multinom}}, \code{\link[stats:nls]{stats::nls}}.
#' 
#' It can be easily extended by implementing three generic functions:
#' \code{\link[glmglrt:fixcoef]{fixcoef}}, \code{\link[glmglrt:vcov_fixcoef]{vcov_fixcoef}} and \code{\link[glmglrt:df_for_wald]{df_for_wald}}.
#' If the implementations of \code{\link[stats:coef]{coef}}, \code{\link[stats:vcov]{vcov}} and \code{\link[stats:df.residual]{df.residual}} are consistent,
#' you do not have to implement \code{fixcoef}, \code{vcov_fixcoef} and \code{df_for_wald}.
#'
#' It also provides \code{method="LRT"} and \code{method="Rao"} on \code{\link[stats:glm]{stats::glm}} and \code{method="LRT"} on
#' negative binomials (\code{\link[MASS:glm.nb]{negbin}}) models.
#' It is implemented by inverting tests performed by \code{\link{p_value_contrast}}, with the help of the Newton-Raphson algorithm
#' on the logit of the two-sided P-value.
#' 
#' @param clevel_logit_tol numeric value; the difference of logit(1-level) that can be tolerated for convergence of the algorithm.
#' @param deriv_rel_SE numeric value; the delta for the numeric derivative, used for the Newton-Raphson algorithm applied to the logit(1-pvalue).
#'   It is expressed as a multiplicative factor for the Standard Error of the contrast.
#'
#' @export
confint_contrast.default=function(model, contrast,
		method=NULL,
		level=0.95,
		alternative=c("two.sided", "less", "greater"),
		clevel_logit_tol=1e-5,
		deriv_rel_SE=1e-4, ...,
		force=FALSE,
		debuglevel=1) {

	check_contrast(model, contrast)
	if (is.null(method)) {
		if (real_glm(model)) {
			method="LRT"
		} else {
			method = "Wald"
		}
	} else {
		method = method[1]
	}
	if (is.null(alternative)) {alternative="two.sided"}
	if (!is.vector(alternative) || !is.character(alternative)) {
		stop("alternative must be a character string")
	}
	alternative = alternative[1]
	if (!(alternative %in% c("two.sided", "less", "greater"))) {
		stop("alternative must be one of: \"two.sided\", \"less\" or \"greater\"")
	}
	
	est = estimate_contrast(model, contrast, method=method, ...)
	if (!(level > 0 & level < 1)) {
		stop("confidence level must be between 0 and 1")
	}
	if (method == "Wald") {
		xlevel=level
	} else {
		if (alternative=="two.sided") {
			xlevel = level/2  # avoid getting Wald's estimations outside of parameter space
		} else {
			xlevel = 0.50+(level-0.50)/2
		}
	}
	ciSE = pvalue_glm_contrast_wald(model, contrast, H0=0, alternative=alternative, force=force, debuglevel=debuglevel, conf.level=xlevel)[c("lower","upper", "SE")]
	ci = ciSE[c("lower","upper")]
	SE = ciSE["SE"]
	if (method == "Wald") {return(ci)}
	if (method == "SlowWald") {method = "Wald"}
	
	if (alternative == "two.sided") {
		target = logit(1-level)
	} else {
		if (level <= 0.50) {
			stop("one-sided confidence level must be greater than 0.50")
		}
		target = logit((1-level)*2)
	}
	root = function(boundary) {
		if (!is.finite(boundary)) {
			stop(paste0("candidate CI boundary must be finite (given: ", boundary, ")"))
		}
		p = p_value_contrast(model, contrast=contrast, method=method, H0=boundary ,alternative="two.sided", debuglevel=0)
		logit(p) - target
	}
	# We are going to find the parameter space validity
	if (alternative!="less") {
		lower=ps_newtonRaphson(root,ci[1], xabstol=Inf, yabstol=clevel_logit_tol, derivdelta=SE*deriv_rel_SE)$root;
	} else {lower=-Inf}
	if (alternative!="greater") {
		upper=ps_newtonRaphson(root,ci[2], xabstol=Inf, yabstol=clevel_logit_tol, derivdelta=SE*deriv_rel_SE)$root;
	} else {upper=Inf}
	names(lower)=NULL;names(upper)=NULL;
	structure(c(lower=lower, upper=upper), method=method)
}

#' Computes point estimates, confidence intervals and P-values of a contrast
#'
#' This function combines outputs from \code{\link{estimate_contrast}}, \code{\link{confint_contrast}} and \code{\link{p_value_contrast}} 
#' to provide a 4-values vector with point estimate (1st value), lower and upper boundaries of the confidence interval (2nd and 3rd values)
#' and P-value (4th value) comparing the contrast to \code{H0}.
#'
#' @param model a fitted statistical model such as a glm or a coxph. 
#' @param contrast numeric vector of the same length as the number of coefficients in the model; it describes the contrast \code{sum(contrast*fixcoef(model))}.
#' @param method character string value; specification of the algorithm used (implementation dependent). NULL must be accepted. The default method is "Wald".
#'  With the default \code{confint_contrast} and \code{p_value_contrast}, the only supported values are: "Wald", "wald" and "SlowWald".
#'  All three are equivalent but "SlowWald" is slower and is used for debug purpose only. If \code{confint_contrast} and \code{p_value_contrast} are
#'  specialized, they may provide other methods.
#' @param level numeric value between 0 and 1; nominal two-sided confidence level of the confidence interval.
#' @param ... Additional parameters that may be used by some implementations.
#' @param debuglevel integer value; set to 0 (default) to disable warnings, 1 to enable warnings and 2 to enable warnings and notes.
#' @param force logical; if TRUE, force computation of P-values in case of convergence problems.
#' @param H0 numeric value; the value of the contrast under the null hypothesis.
#' @param alternative a character string specifying the alternative hypothesis,
#' @family Contrast functions
#' 
#' @details
#' When alternative is "less" or  "greater", a one-sided confidence interval and a one-sided P-value are generated.
#' If H0 is not zero, the P-value compares the estimate to the value of H0, but the estimate and confidence interval are unchanged.
#'
#' @examples
#' data(mtcars)
#' model1 = glm(family="gaussian", data=mtcars, hp ~ 0+factor(gear))
#' # do cars with 5 gears have more horse power (hp) than cars with 4 gears ?
#' estimate_confint_contrast(model1, c(0,-1,1))
#'
#' # now, we fit an equivalent model (same distribution and same predictions)
#' model2 = glm(family=gaussian(log), data=mtcars, hp ~ 0+factor(gear))
#'
#' # do cars with 5 gears have at least twice the horse power than cars with 4 gears ?
#'
#' estimate_confint_contrast(model2, c(0,-1,0.5))
#' @export
estimate_confint_contrast=function(model, contrast, method=NULL, level=0.95, force=FALSE, debuglevel=1, H0=0, alternative=c("two.sided", "less", "greater"), ...) {
	
	if (is.null(method) && real_glm(model)) {
		method="LRT"
	}
	
	est = estimate_contrast(model, contrast, method=method, force=force, debuglevel=debuglevel, H0=H0, alternative=alternative, ...)
	ci  = confint_contrast (model, contrast, method=method, force=force, debuglevel=debuglevel, level=level, H0=H0, alternative=alternative, ...)
	pv  = p_value_contrast (model, contrast, method=method, force=force, debuglevel=debuglevel, level=level, H0=H0, alternative=alternative, ...)
	names(est)=NULL
	structure(c(estimate=est, ci, pv), method=method)
}
