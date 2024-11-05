context("test-pos_def_limits")

test_that("pos_def_limits", {
  expect_error(
    pos_def_limits(.8, .2, .4, NA),
    "you don't have the right number of correlations"
  )
  expect_error(
    pos_def_limits(.8, NA, NA),
    "cors needs to have exactly 1 NA"
  )
  
  pdl <- pos_def_limits(.8, -.5, NA, steps = .1)
  expect_equal(pdl$min, -0.9)
  expect_equal(pdl$max, 0.1)
  
  pdl <- pos_def_limits(.8, .2, NA)
  expect_equal(pdl$min, -0.42)
  expect_equal(pdl$max, 0.74)
  
  pdl <- pos_def_limits(.8, .2, 0, NA, 0, 0)
  expect_equal(pdl$min, -0.42)
  expect_equal(pdl$max, 0.74)
  
  pdl <- pos_def_limits(.8, .2, NA, -.7, 0, 0)
  expect_equal(pdl$min, NA)
  expect_equal(pdl$max, NA)
})