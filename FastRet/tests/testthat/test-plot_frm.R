library(testthat)

test_that("plot_frm works for not-adjusted models", {
    opts <- options(FastRet.mocks = c("train_frm"))
    on.exit(options(opts), add = TRUE)
    frm <- train_frm(df = RP, method = "lasso", seed = 123, nfolds = 2, nw = 1)
    testthat::expect_no_error(object = {
        plot_frm(frm = frm, type = "scatter.cv")
        plot_frm(frm = frm, type = "scatter.train")
    })
    expect_error(
        object = plot_frm(frm = frm, type = "scatter.cv.adj"),
        regexp = "the model has not been adjusted yet"
    )
    expect_error(
        object = plot_frm(frm = frm, type = "scatter.train.adj"),
        regexp = "the model has not been adjusted yet"
    )
})

test_that("plot_frm works for adjusted models", {
    opts <- options(FastRet.mocks = c("train_frm"))
    on.exit(options(opts), add = TRUE)
    frm <- train_frm(df = read_rp_xlsx(), method = "lasso", seed = 123, nfolds = 4, nw = 2)
    frmadj <- adjust_frm(frm, new_data = read_rpadj_xlsx())
    testthat::expect_no_error(object = {
        plot_frm(frm = frmadj, type = "scatter.cv")
        plot_frm(frm = frmadj, type = "scatter.train")
        plot_frm(frm = frmadj, type = "scatter.cv.adj")
        plot_frm(frm = frmadj, type = "scatter.train.adj")
    })
})