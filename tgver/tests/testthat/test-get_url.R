test_that("get_url works", {
  base = "http://localhosthost:3000"
  url = get_url(base)
  expect_true(url == base)
  e = paste0(base, "?dark=false&hideCharts=false")
  url = get_url(base, dark = "false", hideCharts = "false")
  expect_true(url == e)
})
