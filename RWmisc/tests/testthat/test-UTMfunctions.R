test_that("long2UTM works", {
  expect_equal(long2UTM(c(-90, 0, 90)), c(16, 31, 46))
})

test_that("UTMzones works", {
  expect_equal(UTMzones(c(-90, 90, 90)), c(16, 46))
})

test_that("chooseUTM works", {
  expect_equal(chooseUTM(c(-90, -80, -70)), 17)
})
