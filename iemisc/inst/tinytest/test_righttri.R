test_righttri <- function() {

expect_equal(righttri(a = 3, b = 4, c = 5), list(a = 3, b = 4, c = 5, Aangle = 36.8699, Bangle = 53.1301, Cangle = 90), tolerance = 1e-06)
expect_error(righttri(0, 2))
expect_error(righttri(1, 2))
expect_error(righttri(a = 5, c = 10))
expect_error(righttri(a = 3, c = 10))

  invisible(NULL)
}

test_righttri()
