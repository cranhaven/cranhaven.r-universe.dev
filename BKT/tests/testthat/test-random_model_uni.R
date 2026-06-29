library(testthat)
test_that("test-random_model_uni.R", {
    # 示例使用
    # set.seed(123)
    # result <- random_model_uni()
    # print(result)
    num_resources <- 3
    num_subparts <- 3

    # dismiss handle 1
    trans_prior <- t(array(c(20, 4, 1, 20), dim = c(2, 2)))
    trans_prior <- reshape_python(rep(trans_prior, num_resources), dim = c(num_resources, 2, 2))
    target <- reshape_python(c(20, 1, 4, 20, 20, 1, 4, 20, 20, 1, 4, 20), dim = c(num_resources, 2, 2))
    # print_3_dim_matrix(trans_prior)
    # print_3_dim_matrix(target)
    expect_equal(trans_prior, target)

    # dismiss handle 2
    given_notknow_prior <- array(rep(c(5, 0.5), num_subparts), dim = c(2, num_subparts))
    # print(given_notknow_prior)
    expect_equal(given_notknow_prior, reshape_python(c(5, 5, 5, 0.5, 0.5, 0.5), dim = c(2, 3)))

    given_know_prior <- array(rep(c(0.5, 5), num_subparts), dim = c(2, num_subparts))

    pi_0_prior <- matrix(c(100, 1), nrow = 2, byrow = TRUE)

    # model parameter generate
    As <- dirrnd(trans_prior) # ?
    given_notknow <- dirrnd(given_notknow_prior)
    given_know <- dirrnd(given_know_prior)
    pi_0 <- dirrnd(pi_0_prior)
    # print(pi_0)

    # emission
    given_notknow <- reshape_python(c(1, 2, 3, 4, 5, 6), dim = c(2, 3))
    given_know <- reshape_python(c(7, 8, 9, 10, 11, 12), dim = c(2, 3))

    given_notknow_reshaped <- t(matrix(given_notknow, nrow = 2, ncol = num_subparts))
    given_know_reshaped <- t(matrix(given_know, nrow = 2, ncol = num_subparts))
    emissions <- array(c(given_notknow_reshaped, given_know_reshaped),
        dim = c(num_subparts, 2, 2)
    )
    emissions <- aperm(emissions, c(1, 3, 2))
    target <- reshape_python(c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12), dim = c(3, 2, 2))
    expect_equal(emissions, target)

    As <- dirrnd(trans_prior) # ?
    given_notknow <- dirrnd(given_notknow_prior)
    given_know <- dirrnd(given_know_prior)
    pi_0 <- dirrnd(pi_0_prior)
    given_notknow_reshaped <- t(matrix(given_notknow, nrow = 2, ncol = num_subparts))
    given_know_reshaped <- t(matrix(given_know, nrow = 2, ncol = num_subparts))
    emissions <- array(c(given_notknow, given_know),
        dim = c(num_subparts, 2, 2)
    )
    emissions <- aperm(emissions, c(1, 3, 2))
    # As
    modelstruct <- list()
    modelstruct$prior <- runif(1)

    As[, 2, 1] <- runif(num_resources) * 0.40
    As[, 2, 2] <- 1 - As[, 2, 1]
    As[, 1, 2] <- 0
    As[, 1, 1] <- 1

    modelstruct$learns <- As[, 2, 1]
    modelstruct$forgets <- As[, 1, 2]
    given_notknow[2, ] <- runif(num_subparts) * 0.40
    modelstruct$guesses <- given_notknow[2, ]
    given_know[1, ] <- runif(num_subparts) * 0.30
    modelstruct$slips <- given_know[1, ]

    modelstruct$As <- As
    modelstruct$emissions <- emissions
    modelstruct$pi_0 <- pi_0
})
