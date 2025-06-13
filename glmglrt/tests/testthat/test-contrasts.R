data(mtcars)
model1 =  glm(family=binomial(logit), data=mtcars, I(qsec > median(qsec)) ~ gear+wt+mpg)
mtcars2 = mtcars
mtcars2$gear = mtcars2$gear*coef(model1)["gear"]/1
mtcars2$wt = mtcars2$wt*coef(model1)["wt"]/2
mtcars2$mpg = mtcars2$mpg*coef(model1)["mpg"]/4
model2 = glm(family="binomial", data=mtcars2, I(qsec > median(qsec)) ~ gear+wt+mpg)


test_that("LRT contrasts", {


	expect(abs(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="LRT") - 1)<1e-5, "zero contrast -> P-value = 1")
	expect(abs(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="LRT", alternative="less") - 0.5)<1e-5, "zero contrast and alternative=less -> P-value = 0.5")
	expect(abs(p_value_contrast(model2, contrast=c(0,-1,-0.5,1), method="LRT", H0=2) - 1)<1e-5, "contrast effect=2 compared to H0=2 -> P-value = 1")
	expect(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="LRT", H0=2, alternative="less")<0.5, "contrast effect=0 compared to H0=2 with alternative=less -> P-value < 0.5")
	expect(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="LRT", H0=2, alternative="greater")>0.5, "contrast effect=0 compared to H0=2 with alternative=greater -> P-value > 0.5")
})

test_that("Rao contrasts", {

	expect(abs(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="Rao") - 1)<1e-5, "zero contrast -> P-value = 1")
	expect(abs(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="Rao", alternative="less") - 0.5)<1e-5, "zero contrast and alternative=less -> P-value = 0.5")
	expect(abs(p_value_contrast(model2, contrast=c(0,-1,-0.5,1), method="Rao", H0=2) - 1)<1e-5, "contrast effect=2 compared to H0=2 -> P-value = 1")
	expect(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="Rao", H0=2, alternative="less")<0.5, "contrast effect=0 compared to H0=2 with alternative=less -> P-value < 0.5")
	expect(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="Rao", H0=2, alternative="greater")>0.5, "contrast effect=0 compared to H0=2 with alternative=greater -> P-value > 0.5")
})
test_that("Wald contrasts", {

	expect(abs(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="wald") - 1)<1e-5, "zero contrast -> P-value = 1")
	expect(abs(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="wald", alternative="less") - 0.5)<1e-5, "zero contrast and alternative=less -> P-value = 0.5")
	expect(abs(p_value_contrast(model2, contrast=c(0,-1,-0.5,1), method="wald", H0=2) - 1)<1e-5, "contrast effect=2 compared to H0=2 -> P-value = 1")
	expect(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="wald", H0=2, alternative="less")<0.5, "contrast effect=0 compared to H0=2 with alternative=less -> P-value < 0.5")
	expect(p_value_contrast(model2, contrast=c(0,-2,-1,1), method="wald", H0=2, alternative="greater")>0.5, "contrast effect=0 compared to H0=2 with alternative=greater -> P-value > 0.5")
})
