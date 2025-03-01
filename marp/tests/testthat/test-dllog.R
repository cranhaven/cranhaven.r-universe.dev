test_that("dllog", {
    x <- as.numeric(c(350., 450., 227., 352., 654.))
    shape <- 5
    scale <- 3
    log <- FALSE
    result <- marp::dllog(x, shape, scale, log)
    expected_result <- as.numeric(c(6.609490942787837e-13, 1.463191586609840e-13, 8.880167025529705e-12, 6.387343862593732e-13, 1.552779497074985e-14))
    expect_true(all.equal(result, expected_result))

    x <- as.numeric(c(350., 450., 227., 352., 654.))
    shape <- 5
    scale <- 3
    log <- TRUE
    result <- marp::dllog(x, shape, scale, log)
    expected_result <- as.numeric(c(-28.04509957121864, -29.55298614083788, -25.44720074992009, -28.07928769790387, -31.79614475297261))
    expect_true(all.equal(result, expected_result))
})
