test_that("tnf_rosters() test", {
  rosters <- tnf_rosters()
  expect_type(rosters,"list")
  expect_true(nrow(rosters) != 0)
})
