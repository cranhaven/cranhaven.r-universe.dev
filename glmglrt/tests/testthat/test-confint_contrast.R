cisurvreg=function(object, parm=NULL, level=0.95) {
	if (is.null(parm)) {parm = 1:length(coef(object))}
	SE = sqrt(diag(vcov(object)))[parm]
	z = qnorm(c((1-level)/2, 1-(1-level)/2))
	xcoef = coef(object)[parm]
	cbind(xcoef + z[1]*SE, xcoef + z[2]*SE)
}
cilme=function(mod, parm=NULL) {
	m=nlme::intervals(mod)$fixed[,c("lower", "upper")]
	if (!is.null(parm)) {
		m=m[parm,,drop=FALSE]
	}
	return(m)
}
cilmer=function(mod, parm=NULL, level=0.95) {
	SE = sqrt(diag(as.matrix(vcov(mod))))
	z = qt(c((1-level)/2,1-(1-level)/2), df=df.residual(mod))
	m = cbind(fixcoef(mod) + z[1] * SE, fixcoef(mod) + z[2] * SE)
	if (!is.null(parm)) {
		m = m[parm,,drop=FALSE]
	}
	return(m)
}
cimultinom=function(mod, parm=NULL) {
	m = confint(mod)
	m = do.call(rbind,lapply(1:(dim(m)[3]), function(i) {
		m[,,i]
	}))
	if (!is.null(parm)) {
		m=m[parm,,drop=FALSE]
	}
	return(m)
}

consistent_confint_contrast=function(mod, modelname, methods=c("LRT", "Wald", "Rao"), refmethod=NULL) {
	ln = length(fixcoef(mod))
	SEs = sqrt(diag(as.matrix(vcov_fixcoef(mod))))
	for (i in 1:ln) {
		contrast=rep(0, ln)
		contrast[i] = 1
		tolerance = c(2e-2, 2e-2, 1) # expressed in standard errors
		for(imethod in 1:length(methods)) {
			method = methods[imethod]
			test = suppressWarnings(c(estimate_contrast(mod, contrast, method=method), confint_contrast(mod,contrast, method=method)))
			if (!is.null(refmethod)) {
				ci = refmethod(mod, parm=i)
			} else if (method=="Wald") {
				ci = confint.default(mod, parm=i)
			} else {
				ci = suppressMessages(suppressWarnings(confint(mod, parm=i)))
			}
			ref  = c(fixcoef(mod)[i], ci)
			message = paste0("mono-coefficient contrast is inconsistent with coefficient (model name=", modelname, ", method=", method, ", coefficient index=", i, ")")
			expect(all(abs(test[1:3] - ref[1:3]) < tolerance[imethod]*SEs[i]), message)
		}
	}
}

computable_confint_contrast=function(mod, modelname, contrasts, methods=c("LRT", "Wald", "Rao"), level=0.999) {
	if (is.vector(contrasts)) {contrasts=t(contrasts)}
	for(icontrast in 1:nrow(contrasts)) {
	contrast=contrasts[icontrast,]
	for(method in methods) {
		message = paste0(modelname, " (method=", method, ", contrast=c(", paste0(contrast, collapse=", "), "))")
		ci = suppressWarnings(confint_contrast(mod, contrast=contrast, level=level, method=method))
		expect(all(is.finite(ci)), message)
		alpha = 1-level
		p1 = suppressWarnings(p_value_contrast(mod, H0=ci[1], contrast=contrast, method=method))
		p2 = suppressWarnings(p_value_contrast(mod, H0=ci[2], contrast=contrast, method=method))
		expect(is.finite(p1), paste0(message, ": non-finite P-value at lower confidence limit"))
		expect(is.finite(p2), paste0(message, ": non-finite P-value at upper confidence limit"))

		expect(abs(logit(p1) - logit(alpha)) < 1e-2, paste0(message, ": inconsistent P-value at lower confidence limit"))
		expect(abs(logit(p2) - logit(alpha)) < 1e-2, paste0(message, ": inconsistent P-value at upper confidence limit"))
	}
	}
}

computable_confint=function(mod, modelname, level=0.999, methods=c("LRT", "Wald", "Rao")) {
	ln = length(fixcoef(mod))
	for (i in 1:ln) {
		contrast=rep(0, ln)
		contrast[i] = 1
		for (method in methods) {
			computable_confint_contrast(mod, modelname=modelname, contrasts=contrast, methods=method, level=level)
		}
	}
}


test_that("confint_contrast on GLMs", {
	consistent_confint_contrast(glm(family="binomial", rbind(c(500,3),c(500,1))~c(0,1)), "logistic regression with unbalanced intercept")
	consistent_confint_contrast(glm(family=binomial(log), rbind(c(500,5),c(500,5))~c(0,1),start=c(log(500/505),0)), "log-binomial regression with unbalanced intercept")
	consistent_confint_contrast(glm(family=binomial(log), rbind(c(500,3),c(500,3))~c(0,1),start=c(log(500/503),0)), "log-binomial regression with very unbalanced intercept")
	consistent_confint_contrast(glm(family=binomial(log), rbind(c(500,5),c(500,5))~c(0,1e10),start=c(log(500/505),0)), "unbalanced log-binomial regression with tiny coefficients")
	consistent_confint_contrast(glm(family=binomial(log), rbind(c(500e5,5e5),c(500e5,4e5))~c(0,1),start=c(log(500/505),0)), "log-binomial regression with huge sample size")
	expect(all(confint_contrast(glm(family=binomial(log), rbind(c(500,3),c(500,3))~c(0,1),start=c(log(500/503),0)),c(1,0), level=0.9999, method="LRT")<0), "Estimation of an extreme log-binomial intercept with high confidence level and LRT")
	computable_confint(glm(family=binomial(log), rbind(c(500,3),c(500,3))~c(0,1),start=c(log(500/503),0)), "CI estimation of an extreme log-binomial intercept with high confidence level")
	computable_confint_contrast(
		glm(family=binomial(log), rbind(c(500,3),c(500,3))~c(0,1),start=c(log(500/503),0)),
		"CI estimation of an extreme log-binomial intercept with high confidence level",
		rbind(c(1,10), c(1,-1)), method=c("LRT", "Rao"), level=0.95
	)

})
test_that("Errors and warnings on invalid p_value_contrast and confint_contrast on GLMs", {
	mod=glm(family="binomial", rbind(c(500,3),c(500,0))~c(0,1))
	expect_error(confint_contrast(mod,c(1,0),method="LRT"), regexp="outside of parameter space")
	expect_error(confint_contrast(mod,c(1,0),method="Rao"), regexp="outside of parameter space")
	expect_warning(p_value_contrast(mod,c(1,0)), regexp="non-estimable coefficients")
	expect_warning(p_value_contrast(mod,c(0,1)), regexp="too large standard error")
	expect_warning(p_value_contrast(mod,c(1,-1)), regexp="too large standard error")
	mod=glm(family=binomial("logit"), rbind(c(500,50),c(500,50))~c(0,1)+I(c(0,1)))
	expect(all(is.finite(confint_contrast(mod,contrast=c(1,10,0), method="LRT"))), "model with dropped coefficient")
	expect_error(confint_contrast(mod,contrast=c(1,10,1), method="LRT"), "contrast should be zero on NA coefficients")
	expect_error(estimate_contrast(mod, contrast=c(1,10,1), method="LRT"), "contrast should be zero on NA coefficients")
	expect_error(confint_contrast(mod,contrast=c(1,10), method="Wald"), "length of contrast must be the same")
	expect_error(p_value_contrast(mod,contrast=c(1,10), method="Wald"), "length of contrast must be the same")
	expect_error(p_value_contrast(mod,contrast=c(1,10,1), method="Wald"), "contrast should be zero on NA coefficients")

})
test_that("confint_contrast with Wald's formula on many other models", {
	set.seed(2020)
	outcome=rbinom(1000,2,0.5)
	group=rbinom(1000,5,0.5)
	expo=rbinom(1000,1, 0.5)
	y1=rnorm(1000)
	y2=rnorm(1000)
	mod=MASS::polr(ordered(outcome) ~ rep(c(0,1),each=500)+rep(c(0,1),500),Hess=TRUE)
	consistent_confint_contrast(mod, "confint_contrast on MASS::polr model", methods="Wald")

	mod=lm(cbind(y1,y2) ~ expo)
	computable_confint_contrast(mod,"confint_contrast on stats::lm (MLM)",contrasts=c(1,0,1,1), methods="Wald")


	mod=nlme::lme(as.integer(outcome) ~expo, random=~1|group)
	consistent_confint_contrast(mod,"confint_contrast on nlme::lme", methods="Wald", refmethod=cilme)
	mod=nlme::gls(as.integer(outcome) ~expo)
	consistent_confint_contrast(mod,"confint_contrast on nlme::gls", methods="Wald")
	computable_confint(MASS::rlm(outcome ~ expo), "confint_contrast on MASS::rlm", methods="Wald")
	mod=lmerTest::lmer(as.integer(outcome) ~expo+(1|group))

	consistent_confint_contrast(mod,"confint_contrast on lmerTest::lmer", methods="Wald", refmethod=cilmer)

	mod=lme4::glmer(family=binomial(logit), I(outcome>=1) ~expo+(1|group))
	consistent_confint_contrast(mod,"confint_contrast on lme4::glmer", methods="Wald", refmethod=cilmer)

	mod=mgcv::gam(family=binomial(logit), I(outcome>=1) ~expo+s(y1))
	consistent_confint_contrast(mod,"confint_contrast on mgcv::gam", methods="Wald", refmethod=confint.default)

	mod=gam::gam(family=binomial(logit), I(outcome>=1) ~expo+gam::s(y1))
	consistent_confint_contrast(mod,"confint_contrast on gam::gam", methods="Wald", refmethod=confint.default)

	mod=survival::coxph(survival::Surv(abs(y1), outcome>=1) ~ expo+y2)
	consistent_confint_contrast(mod,"confint_contrast on survival::coxph", methods="Wald", refmethod=confint.default)

	mod=survival::survreg(survival::Surv(abs(y1), outcome>=1) ~ expo+y2)
	consistent_confint_contrast(mod,"confint_contrast on survival::survreg", methods="Wald", refmethod=cisurvreg)

	mod=survival::survreg(survival::Surv(abs(y1), outcome>=1) ~ expo+y2, dist="loglogistic")
	consistent_confint_contrast(mod,"confint_contrast on survival::survreg", methods="Wald", refmethod=cisurvreg)

	out=capture.output(mod<-nnet::multinom(factor(outcome) ~ expo+y1+y2, Hess=TRUE))
	consistent_confint_contrast(mod,"confint_contrast on survival::survreg", methods="Wald", refmethod=cimultinom)

	mod=nls(data=data.frame(outcome=outcome, expo=expo), outcome ~ cexpo*expo+inter, start=list(cexpo=0, inter=0))
	consistent_confint_contrast(mod,"confint_contrast on stats::nls", methods="Wald", refmethod=confint.default)

	mod=suppressWarnings(MASS::glm.nb(outcome ~ expo+y1))
	consistent_confint_contrast(mod,"confint_contrast on MASS::glm.nb", methods="Wald", refmethod=confint.default)
	consistent_confint_contrast(mod,"confint_contrast on MASS::glm.nb", methods="LRT", refmethod=function(...) {suppressMessages(confint(...))})

})
