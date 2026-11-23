

test_that("Messages for set_cov are printed only when appropriate", {

    options('psborrow.quiet'=FALSE)

    msg1 <- capture_messages({
        devnull <- set_cov(
            n_cat = 1,
            n_cont = 0,
            mu_int = 0,
            mu_ext = 0,
            var = 1,
            cov = 0.5,
            prob_int = 0.7,
            prob_ext = 0.9
        )
    })
    expect_length(msg1, 0)

    msg2 <- capture_messages({
        devnull <- set_cov(
            n_cat = 2,
            n_cont = 0,
            mu_int = 0,
            mu_ext = 0,
            var = 1,
            cov = 0.5,
            prob_int = 0.7,
            prob_ext = 0.9
        )
    })
    expect_length(msg2, 6)
})

