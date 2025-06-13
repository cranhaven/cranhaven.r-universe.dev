data(mtcars)
override_summary()
tol=1e-4

test_that("null model that converge", {
	smodel = summary(glm(family=binomial(log), rep(1,10) ~ 0, offset=rep(log(0.9),10)))

	expect(is.matrix(smodel$extra$pvalues), "summary.glm()$extra$pvalues must be a matrix")
	expect("LRT P-value" %in% colnames(smodel$extra$pvalues), "LRT P-value has not been added")
	expect(nrow(smodel$extra$pvalues) == 0, "zero coefficients are expected")
})

test_that("missing outcome", {
	mtcars$mpg[2]=NA
	model = glm(data=mtcars, mpg ~ gear)
	expect_equal(nrow(summary(model)$extra$pvalues), 2)
})
test_that("model with dropped coefficients", {
	model = glm(data=mtcars, disp ~ 1+gear+I(gear)+drat+wt+I(drat))

	expect_equal(nrow(summary(model)$extra$pvalues), 4)
	x=capture.output(print(summary(model)))
})

test_that("logistic model with enormous variances", {
	model = glm(family="binomial", rbind(c(5,0),c(5,5)) ~ c(0,1))

	expect_warning(capture.output(print(summary(model))), "huge standard errors|converge")
})

test_that("model that do not converge", {
	model = expect_warning((glm(family=binomial(log), data=mtcars, I(disp > median(disp)) ~ 0+qsec+wt,start=c(-0.1,0.3))))

	expect_warning(capture.output(print(summary(model))), "converge")

	expect(all(is.na(summary(model,debuglevel=0)$extra$pvalues[,"LRT P-value"])), "LRT P-values must not be computed when the model does not converge")
	expect(!all(is.na(summary(model,debuglevel=0,force=TRUE)$extra$pvalues[,"LRT P-value"])), "LRT P-values must be computed even when the model does not converge if force=TRUE")
	expect_warning(summary(model),"converge")
})

test_that("command-line offset is taken in account", {
	model1 = glm(family="gaussian", data=mtcars, mpg ~ gear)
	model2 = glm(family="gaussian", data=mtcars, mpg ~ gear, offset=coef(model1)[2]*gear)
	expect_equal(summary(model2)$extra$pvalues["gear","LRT P-value"],1,tolerance=tol)
})

test_that("formula offset is taken in account", {
	model1 = glm(family="gaussian", data=mtcars, mpg ~ gear)
	model2 = glm(family="gaussian", data=mtcars, mpg ~ gear+offset(coef(model1)[2]*gear))
	expect_equal(summary(model2)$extra$pvalues["gear","LRT P-value"],1,tolerance=tol)
})

test_that("command-line+formula offsets are taken in account", {
	model1 = glm(family="gaussian", data=mtcars, mpg ~ gear)
	model2 = glm(family="gaussian", data=mtcars, mpg ~ gear+offset(coef(model1)[2]*gear/2), offset=coef(model1)[2]*gear/2)
	expect_equal(summary(model2)$extra$pvalues["gear","LRT P-value"],1,tolerance=tol)
})

test_that("weights are taken in account", {
	model1 = glm(family="gaussian", data=mtcars, mpg ~ 1, weights=wt) # weighted mean
	model2 = glm(family="gaussian", data=mtcars, I(mpg-coef(model1)) ~ 1, weights=wt)
	expect_equal(summary(model2)$extra$pvalues["(Intercept)","LRT P-value"],1,tolerance=tol)
})
test_that("weights and offset work together in intercept-only model", {
	model1 = glm(family="gaussian", data=mtcars, mpg ~ 1, weights=wt) # weighted mean
	model2 = glm(family="gaussian", data=mtcars, I(mpg) ~ 1, weights=wt, offset=fitted(model1))
	expect_equal(summary(model2)$extra$pvalues["(Intercept)","LRT P-value"],1,tolerance=tol)

})

test_that("LRT and Rao tests equal on gaussian model", {
	model = glm(family="gaussian", data=mtcars, mpg ~ gear)
	smodel = summary(model, method=c("Rao","LRT", "Wald"))$extra$pvalues
	expect_equal(smodel["LRT P-value"], smodel["Rao P-value"],tolerance=tol)
})

test_that("Wald P-values are equal on gaussian, Gamma and inverse.gaussian model", {
	for(fam in c("gaussian", "Gamma", "inverse.gaussian")) {
		for(lnk in c("log","identity","sqrt","inverse","1/mu^2")) {
			model = glm(family=get(fam)(lnk), data=mtcars, mpg ~ gear)
			smodel = summary(model, method=c("Rao","LRT", "Wald"))
			expect(all(is.finite(smodel$extra$pvalues[,"LRT P-value"])), "LRT P-value must be well defined")
			expect(all(is.finite(smodel$extra$pvalues[,"Rao P-value"])), "Rao P-value must be well defined")
			expect_equal(smodel$extra$pvalues[,"Wald P-value"], smodel$coefficients[,"Pr(>|t|)"],tolerance=tol)
		}
	}
})
test_that("Wald P-values are equal on binomial models", {
	for(fam in c("binomial")) {
		for(lnk in c("logit","log","identity","probit","cauchit","cloglog")) {
			off = get(fam)(lnk)$linkfun(0.5)
			model = glm(family=get(fam)(lnk), data=mtcars, I(mpg>median(mpg)) ~ 1,offset=rep(off,nrow(mtcars)))
			smodel = summary(model, method=c("Wald","LRT","Rao"))
			expect(all(is.finite(smodel$extra$pvalues[,"LRT P-value"])), "LRT P-value must be well defined")
			expect(all(is.finite(smodel$extra$pvalues[,"Rao P-value"])), "Rao P-value must be well defined")
			expect_equal(smodel$extra$pvalues[,"Wald P-value"], smodel$coefficients[,"Pr(>|z|)"],tolerance=tol)
		}
	}
})
test_that("Wald P-values are equal on poisson models", {
	for(fam in c("poisson")) {
		for(lnk in c("log", "identity","sqrt","inverse","1/mu^2")) {
			off = get(fam)(lnk)$linkfun(1)
			model = glm(family=get(fam)(lnk), data=mtcars, I(mpg>median(mpg)) ~ 1,offset=rep(off,nrow(mtcars)))
			smodel = summary(model, method=c("Wald","LRT","Rao"))
			expect(all(is.finite(smodel$extra$pvalues[,"LRT P-value"])), "LRT P-value must be well defined")
			expect(all(is.finite(smodel$extra$pvalues[,"Rao P-value"])), "Rao P-value must be well defined")
			expect_equal(smodel$extra$pvalues[,"Wald P-value"], smodel$coefficients[,"Pr(>|z|)"],tolerance=tol)
		}
	}
})
test_that("LRT produces P-values in negative binomial models", {
	model=MASS::glm.nb(data=mtcars, I(cyl*gear) ~ 1)
	pvalues = summary(model)$extra$pvalues[,"LRT P-value"]
	expect(all(!is.na(pvalues)) & length(pvalues)==1, "one non-NA P-value must be generated for intercept-only negative binomial model")
})
test_that("LRT produces P-values in negative binomial models with sqrt link", {
	model=MASS::glm.nb(data=mtcars, I(cyl*gear) ~ 1+gear, link="sqrt")
	pvalues = summary(model)$extra$pvalues[,"LRT P-value"]
	expect(all(!is.na(pvalues)) & length(pvalues)==2, "one non-NA P-value must be generated for intercept-only negative binomial model")
})

test_that("lm models are not affected", {
	expect_false("extra" %in% names(summary(lm(I(1:3) ~ 1))))
})

test_that("non-standard fit methods are passed-through", {
	glm.fit2=function(...) {
		message("glm.fit2 called")
		return(glm.fit(...))
	}
	model = expect_message(glm(family="binomial", data=mtcars, I(cyl > median(cyl))~1 ,method=glm.fit2),"glm.fit2 called")
	expect_message(summary(model),"glm.fit2 called")
})


