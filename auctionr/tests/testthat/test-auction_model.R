context("Auction model")

test_that("Requires appropriate parameters", {
  obs = 100
  mu = 5
  alpha = 1
  sigma = .6
  exd <- readRDS("exd.rds")

  expect_error(auction_model(),
               "Argument 'dat' is required")

  # Requires numeric inputs
  expect_error(auction_model(exd, init_param = c(mu, alpha, sigma, 0, 0, 0, 0, 0)),
               "column.+must be numeric")

  # Must have init_param values for everything
  expect_error(auction_model(exd, init_param = c(mu, alpha, sigma, 0, 0, 0, 0)),
               "Argument.+must be of length")

  ## mu, alpha, sigma must be positive
  expect_error(auction_model(exd[1:4], init_param = c(mu, -1, sigma, 0, 0)),
               ".*must be positive")

  # This one should work
  skip_on_cran()
  expect_error(m1 <- auction_model(exd[, 1:5], init_param = c(mu, alpha, sigma, 0, 0, 0), num_cores = 1),
               NA)
  expect_equal(length(m1$par), 6)
  expect_match(paste(capture.output(auction_model(exd[, 1:5],
                                                  init_param = c(mu, alpha, sigma, 0, 0, 0),
                                                  num_cores = 1, std_err = TRUE)),
                     collapse = "\n"),
               "\\(.*\\d+\\)")
  expect_match(paste(capture.output(auction_model(exd[, 1:5],
                                                  init_param = c(mu, alpha, sigma, 0, 0, 0),
                                                  num_cores = 1, std_err = FALSE)),
                     collapse = "\n"),
               "\\(--\\)")
}
)
