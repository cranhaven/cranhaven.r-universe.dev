#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}

test_that("assess formula manipulation", {
  res <- update_formula_and_data(
    formula = Y ~ X +
      amp_acro(time, n_components = 2, group = "X", period = c(12, 10)),
    data = vitamind
  )

  expect_true(all.equal(
    res$newformula,
    Y ~ X + X:main_rrr1 + X:main_sss1 + X:main_rrr2 + X:main_sss2
  ))
})

test_that("warning for missing data", {
  vitamind_test <- vitamind
  vitamind_test$Y[1] <- NA

  suppressMessages(expect_message(
    update_formula_and_data(
      formula = Y ~ X +
        amp_acro(time, n_components = 2, group = "X", period = c(12, 10)),
      data = vitamind_test,
      quietly = FALSE
    )
  ))
})
