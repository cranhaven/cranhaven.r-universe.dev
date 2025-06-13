
test_that("LRT p-values", {
	model1 = glm(family="binomial", data=mtcars, I(qsec > median(qsec)) ~ gear+wt+mpg)
	df = p_value(model1,test="LRT")
	expect(is.data.frame(df),"p_value must return a data.frame")
	expect(all(!is.na(df)), "p_value must not generate NA on this model")
	expect_equal(nrow(df),4)
	expect_equal(ncol(df),2)
	expect_equal(df[,1],names(coef(model1)))
})

test_that("Wald p-values", {
	model1 = glm(family="binomial", data=mtcars, I(qsec > median(qsec)) ~ gear+wt+mpg)
	df = p_value(model1,test="Wald")
	expect(is.data.frame(df),"p_value must return a data.frame")
	expect(all(!is.na(df)), "p_value must not generate NA on this model")
	expect_equal(nrow(df),4)
	expect_equal(ncol(df),2)
	expect_equal(df[,1],names(coef(model1)))
})
