test_colebrook <- function() {

expect_equal(colebrook(c(3e3, 7e5, 1e100), 0.01), c(4.351919e-02, 1.238992e-02, 2.640067e-05), tolerance = 9e-08)
expect_equal(colebrook(2301), 0.04727678, tolerance = 9e-08)
expect_error(colebrook(2300))
expect_error(colebrook(NA))
expect_error(colebrook("sq"))

  invisible(NULL)
}

test_colebrook()
