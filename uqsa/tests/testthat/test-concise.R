test_that("parenthesized error notation, normal",{
	require(errors)
	x <- parse_concise(c("12(3)","-12(3)","12.0(3)","-12.0(3)","12(3)e4","12(3)e-4"))
	expect_equal(as.numeric(x),c(12,-12,12,-12,12e4,12e-4))
	expect_equal(errors(x),c(3,3,0.3,0.3,3e4,3e-4))
})

test_that("parenthesized error notation, weird input",{
	x <- parse_concise(c("123(456)","123","12.0(3","12(3"))
	expect_equal(as.numeric(x),c(123,123,12,12))
	expect_equal(errors(x),c(456,0,0.3,3))
})

test_that("not parenthesized error notation, with a semicolon",{
	x <- parse_concise(c("12;3","12e3;3e2","12;0.3","-12;3"))
	expect_equal(as.numeric(x),c(12,12e3,12,-12))
	expect_equal(errors(x),c(3,3e2,0.3,3))
})

