test_that("basic patterns can be applied", {

	x <- tm(witch ~ wicked + west)
	yd <- apply_pattern(x, "direct")
	expect_length(yd, 3)
	ys <- apply_pattern(x, "sequential")
	expect_length(ys, 3)
	expect_length(ys$outcome, 2)
	yp <- apply_pattern(x, "parallel")
	expect_length(yp, 2)
	expect_named(yp, c("outcome", "covariate_1"))
	yf <- apply_pattern(x, "fundamental")
	expect_length(yf, 2)
	expect_named(yf, c("left", "right"))

	x <- tm(witch ~ wicked + west + green)
	y <- apply_sequential_pattern(x)
	expect_named(y, c("outcome", paste0("covariate_", 1:3)))

	x <- tm(witch ~ .x(wicked) + west + green)
	y <- apply_parallel_pattern(x)
	expect_named(y, c("outcome", "exposure", "covariate_1"))
	expect_length(y, 3)
	expect_length(y$exposure, 2)
	expect_length(unique(y$exposure), 1)

	# Sequential with restricted terms
	x <- tm(witch ~ wicked)
	ys <- apply_sequential_pattern(x)
	yp <- apply_parallel_pattern(x)
	yf <- apply_fundamental_pattern(x)
	yd <- apply_direct_pattern(x)

})

test_that("interaction terms can be rolled through a formula", {

	x <- tm(wicked ~ .x(witch) + west + .i(green) + magic + hat)
	fmls(x, pattern = "parallel")

})
