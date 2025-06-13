data(mtcars)

mtcars$sdzkjoiu = mtcars$wt
model = glm(data=mtcars, hp ~ sdzkjoiu)

capture.print=function(model, ...) {
	out = capture.output(print(summarylr(model),...))
	out = out[grepl("^sdzkjoiu", out)]
	return(out)
}
test_that("print.summary.glmglrt honors digits", {
	out = capture.print(model, digits=10)
	expect(grepl("[0-9]{8,}", out), "output has many digits")
})

test_that("print.summary.glmglrt honors signif.stars", {
	out1 = capture.print(model, signif.stars=TRUE)
	out2 = capture.print(model, signif.stars=FALSE)
	expect(grepl("[*][*][*]", out1),"when signif.stars=TRUE, stars must appear")
	expect(!grepl("[*][*][*]", out2),"when signif.stars=FALSE, stars must not appear")
})

