context("fuzzy_join")

test_that("Can join multiple times to the same column", {
  ret <- fuzzy_inner_join(mtcars, mtcars,
                          by = c("gear" = "cyl", "carb" = "cyl"),
                          match_fun = list(`==`, `==`))

  expect_gt(nrow(ret), 0)
  expect_equal(ret$gear.x, ret$cyl.y)
  expect_equal(ret$carb.x, ret$cyl.y)
})

test_that("fuzzy_join supports formula notation", {
  ret1 <- fuzzy_inner_join(mtcars, mtcars, by = "wt", match_fun = ~ .x > .y)
  expect_true(all(ret1$wt.x > ret1$wt.y))

  ret2 <- fuzzy_inner_join(mtcars, mtcars, by = "wt", match_fun = ~ .x <= .y)
  expect_true(all(ret2$wt.x <= ret2$wt.y))

  # These two tables should, between them, include all combinations
  expect_equal(nrow(ret1) + nrow(ret2), nrow(mtcars) ^ 2)

  # Try multiple match functions
  ret3 <- fuzzy_inner_join(mtcars,
                           mtcars,
                           by = c("wt", "mpg"),
                           match_fun = list(~ .x > .y, ~ .x - 3 > .y))

  expect_true(all(ret3$wt.x > ret3$wt.y))
  expect_true(all(ret3$mpg.x - 3 > ret3$mpg.y))
})
