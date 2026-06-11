context("Generate data")

test_that("Requires appropriate parameters, obs, nbids", {
  obs = 100
  mu = 5
  alpha = 3
  sigma = .6
  # Requires parameters
  expect_error(auction_generate_data(obs = obs),
               "Argument.+required")
  # Requires obs
  expect_error(auction_generate_data(mu = mu,
                                     alpha = alpha,
                                     sigma = sigma),
               "Argument.+required")
  # Requires numeric inputs
  expect_error(auction_generate_data(obs = "a",
                                     mu = mu,
                                     beta = 0.5,
                                     alpha = alpha,
                                     sigma = sigma,
                                     new_x_mean = 1,
                                     new_x_sd = 1),
               "Argument.+must be numeric")
  # Requires positive mu, alpha, sigma
  expect_error(auction_generate_data(obs = "a",
                                     mu = -1,
                                     beta = 0.5,
                                     alpha = alpha,
                                     sigma = sigma,
                                     new_x_mean = 1,
                                     new_x_sd = 1),
               ".+must be positive")
  ## new_x_mean and new_x_sd must have same length as beta
  expect_error(auction_generate_data(obs = obs,
                                     mu = mu,
                                     beta = 0.5,
                                     alpha = alpha,
                                     sigma = sigma,
                                     new_x_mean = 1:2,
                                     new_x_sd = 1:2),
               "must have the same length")
  # Output dimension depends on obs and beta
  expect_equal(dim(auction_generate_data(obs = 10,
                                     mu = mu,
                                     beta = 0.5,
                                     alpha = alpha,
                                     sigma = sigma,
                                     new_x_mean = 1,
                                     new_x_sd = 1)),
               c(10, 3))
  expect_equal(dim(auction_generate_data(obs = 19,
                                         mu = mu,
                                         beta = c(.3, 0.5, .8),
                                         alpha = alpha,
                                         sigma = sigma,
                                         new_x_mean = 1:3,
                                         new_x_sd = 1:3)),
               c(19, 5))

  # N_bids should not exceed max_n_bid
  out <- auction_generate_data(obs = 20,
                               mu = mu,
                               max_n_bid = 5,
                                beta = 0.5,
                                alpha = alpha,
                                sigma = sigma,
                                new_x_mean = 1,
                                new_x_sd = 1)
  expect_lte(min(out$n_bids), 5)
}
)
