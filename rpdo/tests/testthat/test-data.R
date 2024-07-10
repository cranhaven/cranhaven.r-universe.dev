test_that("pdo", {
  expect_is(check_pdo(rpdo::pdo), "data.frame")
})
