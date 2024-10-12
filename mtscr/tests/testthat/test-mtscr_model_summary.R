test_that("model is summarised", {
  summary <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3) |>
    mtscr_model_summary()

  expect_equal(dim(summary), c(3, 10))
})

# Test that model must be a glmmTMB object or a list of glmmTMB objects
test_that("model must be a glmmTMB object or a list of glmmTMB objects", {
  expect_error(mtscr_model_summary(1), regexp = "glmmTMB")
  expect_error(mtscr_model_summary(list(1:3)), regexp = "glmmTMB")
})
