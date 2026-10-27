test_that("xc_rosters() test", {
  rosters <- xc_rosters()
  expect_type(rosters,"list")
  expect_true(nrow(rosters) != 0)
})
