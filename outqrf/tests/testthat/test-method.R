test_that("plot() works normally in iris", {
    qrf <- outqrf(iris)
    expect_no_error(plot(qrf))
})
