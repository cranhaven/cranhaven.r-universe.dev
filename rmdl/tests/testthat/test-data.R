test_that("data utils work", {
	x <- mtcars$carb
	x[2:3] <- NA
	expect_equal(number_of_missing(x), 2)

})
