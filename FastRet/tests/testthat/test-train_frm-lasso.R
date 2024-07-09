library(testthat)

test_that("train_frm works", {
    opts <- options(FastRet.mocks = c("preprocess_data"))
    on.exit(options(opts), add = TRUE)
    model <- train_frm(df = RP, method = "lasso", nfolds = 2, nw = 2, verbose = 0)
    expect_equal(names(model), c("model", "df", "cv", "seed", "version"))
})
