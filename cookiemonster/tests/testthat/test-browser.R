test_that("get cookies from Firefox", {
  skip_if(length(find_cookiejar("Firefox")) == 0L)
  skip_on_cran()
  cookies <- get_cookies_from_browser()
  expect_equal(ncol(cookies), 7)
  expect_gt(nrow(cookies), 1L)
})
