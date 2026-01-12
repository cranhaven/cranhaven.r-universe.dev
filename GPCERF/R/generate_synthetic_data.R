#' @title
#' Generate synthetic data for the GPCERF package
#'
#' @description
#' Generates synthetic data set based on different GPS models and covariates.
#'
#' @param sample_size A number of data samples.
#' @param outcome_sd Standard deviation used to generate the outcome in the
#' synthetic data set.
#' @param gps_spec A numeric value (1-6) that indicates the GPS model used to
#' generate the continuous exposure.
#' @param cova_spec A numeric value (1-2) to modify the covariates.
#'
#' @return
#' A data frame of the synthetic data. Outcome is labeled as Y, exposure as w,
#' and covariates cf1-6.
#'
#' @export
#'
#' @examples
#'
#' set.seed(351)
#' data <- generate_synthetic_data(sample_size = 200)
#'
generate_synthetic_data <- function(sample_size = 1000, outcome_sd = 10,
                                   gps_spec = 1, cova_spec = 1) {

  # pre-treatment variables (confounders)
  cf <- MASS::mvrnorm(n = sample_size, mu = rep(0, 4), Sigma = diag(4))
  cf5 <- sample(c((-2):2), sample_size, replace = TRUE)
  cf6 <- stats::runif(sample_size, min = -3, max = 3)

  if (gps_spec == 1) {
    treat <- (-0.8 + 0.1 * cf[, 1] +
                     0.1 * cf[, 2] -
                     0.1 * cf[, 3] +
                     0.2 * cf[, 4] +
                     0.1 * cf5 + 0.1 * cf6) * 9 + 17 +
                     stats::rnorm(sample_size, sd = 5)
  } else if (gps_spec == 2) {
    treat <- (-0.8 + 0.1 * cf[, 1] +
                     0.1 * cf[, 2] -
                     0.1 * cf[, 3] +
                     0.2 * cf[, 4] +
                     0.1 * cf5 + 0.1 * cf6) * 15 + 22 +
                     stats::rt(sample_size, 2)
    treat[which(treat < (-5))] <- (-5)
    treat[which(treat > (25))] <- (25)
  }else if (gps_spec == 3) {
    treat <- (-0.8 + 0.1 * cf[, 1] +
                     0.1 * cf[, 2] -
                     0.1 * cf[, 3] +
                     0.2 * cf[, 4] +
                     0.1 * cf5 +
                     0.1 * cf6) * 9 + 1.5 * cf[, 3]^2 +
                     stats::rnorm(sample_size, mean = 0, 5) + 15
  }else if (gps_spec == 4) {
    treat <- 49 * exp((-0.8 + 0.1 * cf[, 1] +
                              0.1 * cf[, 2] -
                              0.1 * cf[, 3] +
                              0.2 * cf[, 4] +
                              0.1 * cf5 +
                              0.1 * cf6)) /
                (1 + exp((-0.8 + 0.1 * cf[, 1] +
                                 0.1 * cf[, 2] -
                                 0.1 * cf[, 3] +
                                 0.2 * cf[, 4] +
                                 0.1 * cf5 +
                                 0.1 * cf6))) - 6 +
                         stats::rnorm(sample_size, sd = 5)
  }else if (gps_spec == 5) {
    treat <- 42 / (1 + exp((-0.8 + 0.1 * cf[, 1] +
                                   0.1 * cf[, 2] -
                                   0.1 * cf[, 3] +
                                   0.2 * cf[, 4] +
                                   0.1 * cf5 +
                                   0.1 * cf6))) - 18 +
                       stats::rnorm(sample_size, sd = 5)
  }else if (gps_spec == 6) {
    treat <- log(abs(-0.8 + 0.1 * cf[, 1] +
                            0.1 * cf[, 2] -
                            0.1 * cf[, 3] +
                            0.2 * cf[, 4] +
                            0.1 * cf5 + 0.1 * cf6)) * 7 + 13 +
             stats::rnorm(sample_size, sd = 4)
  }

  #produce outcome Y
  Y <- sapply(1:sample_size, function(i) {
      -10 - 5 * sum(c(2, 2, 3, -1) * cf[i, ]) - 5 * 2 * cf5[i] - 5 * 2 * cf6[i] -
      treat[i] * (0.1 - 0.1 * cf[i, 1] + 0.1 * cf[i, 4] + 0.1 * cf5[i] +
                  0.1 * cf[i, 3] ^ 2) * 5 + 0.13 ^ 2 * treat[i] ^ 3 +
                  stats::rnorm(1, mean = 0, sd = outcome_sd)
  })
  if (cova_spec == 1) {
    cf <- cf
    #Kang 2007
  } else if (cova_spec == 2) {
    cf[, 1] <- exp(cf[, 1] / 2)
    cf[, 2] <- (cf[, 2] / (1 + exp(cf[, 1]))) + 10
    cf[, 3] <- (cf[, 1] * cf[, 3] / 25 + 0.6) ^ 3
    cf[, 4] <- (cf[, 2] + cf[, 4] + 20) ^ 2
  }
  simulated_data <- data.frame(cbind(Y, treat, cf, cf5, cf6))
  colnames(simulated_data)[3:8] <- c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6")
  return(simulated_data)
}
