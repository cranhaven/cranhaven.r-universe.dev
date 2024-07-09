library(testthat)

test_that("adjust_frm works", {
    opts <- options(FastRet.mocks = c("train_frm"))
    on.exit(options(opts), add = TRUE)
    frm = train_frm(verbose = 0)
    new_data = read_rpadj_xlsx()
    m12 <- adjust_frm(frm, new_data, predictors = 1:2)
    m1to6 <- adjust_frm(frm, new_data, predictors = 1:6)
    expect_identical(
        object = names(coef(m12$adj$model)),
        expected = c("(Intercept)", "RT", "I(RT^2)")
    )
    expect_identical(
        object = names(coef(m1to6$adj$model)),
        expected = c("(Intercept)", "RT", "I(RT^2)", "I(RT^3)", "log(RT)", "exp(RT)", "sqrt(RT)")
    )
    expect_error(
        object = adjust_frm(frm, new_data, predictors = c()),
        regexp = ".*Invalid predictors.*"
    )
})
