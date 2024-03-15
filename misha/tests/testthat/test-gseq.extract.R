test_that("gseq.extract with gscreen results on test.fixedbin", {
    intervs <- gscreen("test.fixedbin > 0.6", gintervals(c(1, 2, 3)))
    expect_regression(gseq.extract(intervs), "gseq_extract_gscreen_fixedbin")
})

test_that("gseq.extract with modified strand in gscreen results on test.fixedbin", {
    intervs <- gscreen("test.fixedbin > 0.6", gintervals(c(1, 2, 3)))
    intervs$strand <- -1
    expect_regression(gseq.extract(intervs), "gseq_extract_gscreen_fixedbin_modified_strand")
})

test_that("gseq.extract with gintervals.2d", {
    expect_error(gseq.extract(gintervals.2d(1, 10, 100, 2, 20, 300)))
})
