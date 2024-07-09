library(testthat)

test_that("selective_measuring works", {
    opts <- options(FastRet.mocks = c("preprocess_data"))
    on.exit(options(opts), add = TRUE)
    obj <- selective_measuring(raw_data = read_rp_xlsx(), k_cluster = 25)
    expect_true(all(names(obj) == c("clustering", "clobj", "coefs", "model", "df", "dfz", "dfzb")))
})
