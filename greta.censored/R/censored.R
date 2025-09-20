library(reticulate)

distrib <- greta:::.internals$nodes$constructors$distrib
distribution_node <- greta:::.internals$nodes$node_classes$distribution_node
as.greta_array <- greta:::.internals$greta_arrays$as.greta_array

# check_dims <- greta:::.internals$checks$check_dims
# check_numeric_length_1 <- greta:::.internals$checks$check_numeric_length_1
# check_finite <- greta:::.internals$checks$check_finite
# check_x_gte_y <- greta:::.internals$checks$check_x_gte_y
# check_param_greta_array <- greta:::.internals$checks$check_param_greta_array

tf <- NULL
tfp <- NULL

initialize_tf <- function() {
  if (is.null(tf) || is.null(tfp)) {
    if (reticulate::py_module_available("tensorflow") &&
      reticulate::py_module_available("tensorflow_probability")) {
      tf <<- reticulate::import("tensorflow", delay_load = TRUE)
      tfp <<- reticulate::import("tensorflow_probability", delay_load = TRUE)
    }
  }
}

tfp <- import("tensorflow_probability", delay_load = TRUE)
tf <- import("tensorflow", delay_load = TRUE)

normal_censored_distribution <- R6::R6Class(
  "normal_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(mean, sd, is_censored, censor, lower, upper, dim) {
      mean <- as.greta_array(mean)
      sd <- as.greta_array(sd)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(mean, sd, is_censored, target_dim = dim)

      super$initialize("normal_censored", dim)
      self$add_parameter(mean, "mean")
      self$add_parameter(sd, "sd")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      mean <- parameters$mean
      sd <- parameters$sd
      is_censored <- parameters$is_censored
      norm_dist <- tfp$distributions$Normal(loc = mean, scale = sd)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) norm_dist$log_survival_function(y),
        "left" = function(y) norm_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- norm_dist$log_cdf(self$upper)
          log_cdf_lower <- norm_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) norm_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) norm_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

lognormal_censored_distribution <- R6::R6Class(
  "lognormal_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(meanlog, sdlog, is_censored, censor, lower, upper, dim) {
      meanlog <- as.greta_array(meanlog)
      sdlog <- as.greta_array(sdlog)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(meanlog, sdlog, is_censored, target_dim = dim)

      super$initialize("lognormal_censored", dim)
      self$add_parameter(meanlog, "meanlog")
      self$add_parameter(sdlog, "sdlog")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      meanlog <- parameters$meanlog
      sdlog <- parameters$sdlog
      is_censored <- parameters$is_censored
      lognorm_dist <- tfp$distributions$LogNormal(loc = meanlog, scale = sdlog)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) lognorm_dist$log_survival_function(y),
        "left" = function(y) lognorm_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- lognorm_dist$log_cdf(self$upper)
          log_cdf_lower <- lognorm_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) lognorm_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) lognorm_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

student_censored_distribution <- R6::R6Class(
  "student_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(df, loc, scale, is_censored, censor, lower, upper, dim) {
      df <- as.greta_array(df)
      loc <- as.greta_array(loc)
      scale <- as.greta_array(scale)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(df, loc, scale, is_censored, target_dim = dim)

      super$initialize("student_censored", dim)
      self$add_parameter(df, "df")
      self$add_parameter(loc, "loc")
      self$add_parameter(scale, "scale")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      df <- parameters$df
      loc <- parameters$loc
      scale <- parameters$scale
      is_censored <- parameters$is_censored
      student_dist <- tfp$distributions$StudentT(df = df, loc = loc, scale = scale)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) student_dist$log_survival_function(y),
        "left" = function(y) student_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- student_dist$log_cdf(self$upper)
          log_cdf_lower <- student_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) student_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) student_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

gamma_censored_distribution <- R6::R6Class(
  "gamma_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(shape, rate, is_censored, censor, lower, upper, dim) {
      shape <- as.greta_array(shape)
      rate <- as.greta_array(rate)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(shape, rate, is_censored, target_dim = dim)

      super$initialize("gamma_censored", dim)
      self$add_parameter(shape, "shape")
      self$add_parameter(rate, "rate")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      shape <- parameters$shape
      rate <- parameters$rate
      is_censored <- parameters$is_censored
      gamma_dist <- tfp$distributions$Gamma(concentration = shape, rate = rate)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) gamma_dist$log_survival_function(y),
        "left" = function(y) gamma_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- gamma_dist$log_cdf(self$upper)
          log_cdf_lower <- gamma_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) gamma_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) gamma_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

exponential_censored_distribution <- R6::R6Class(
  "exponential_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(rate, is_censored, censor, lower, upper, dim) {
      rate <- as.greta_array(rate)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(rate, is_censored, target_dim = dim)

      super$initialize("exponential_censored", dim)
      self$add_parameter(rate, "rate")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      rate <- parameters$rate
      is_censored <- parameters$is_censored
      exp_dist <- tfp$distributions$Exponential(rate = rate)
      censored_log_prob <- switch(self$censor,
        "right" = function(y) exp_dist$log_survival_function(y),
        "left" = function(y) exp_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- exp_dist$log_cdf(self$upper)
          log_cdf_lower <- exp_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) exp_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) exp_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

weibull_censored_distribution <- R6::R6Class(
  "weibull_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(shape, scale, is_censored, censor, lower, upper, dim) {
      shape <- as.greta_array(shape)
      scale <- as.greta_array(scale)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(shape, scale, is_censored, target_dim = dim)

      super$initialize("weibull_censored", dim)
      self$add_parameter(shape, "shape")
      self$add_parameter(scale, "scale")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      shape <- parameters$shape
      scale <- parameters$scale
      is_censored <- parameters$is_censored
      weibull_dist <- tfp$distributions$Weibull(concentration = shape, scale = scale)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) weibull_dist$log_survival_function(y),
        "left" = function(y) weibull_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- weibull_dist$log_cdf(self$upper)
          log_cdf_lower <- weibull_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) weibull_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) weibull_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

pareto_censored_distribution <- R6::R6Class(
  "pareto_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(scale, alpha, is_censored, censor, lower, upper, dim) {
      scale <- as.greta_array(scale)
      alpha <- as.greta_array(alpha)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(scale, alpha, is_censored, target_dim = dim)

      super$initialize("pareto_censored", dim)
      self$add_parameter(scale, "scale")
      self$add_parameter(alpha, "alpha")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      scale <- parameters$scale
      alpha <- parameters$alpha
      is_censored <- parameters$is_censored
      pareto_dist <- tfp$distributions$Pareto(concentration = alpha, scale = scale)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) pareto_dist$log_survival_function(y),
        "left" = function(y) pareto_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- pareto_dist$log_cdf(self$upper)
          log_cdf_lower <- pareto_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) pareto_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) pareto_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

beta_censored_distribution <- R6::R6Class(
  "beta_censored_distribution",
  inherit = distribution_node,
  public = list(
    censor = NULL,
    lower = NULL,
    upper = NULL,
    initialize = function(alpha, beta, is_censored, censor, lower, upper, dim) {
      alpha <- as.greta_array(alpha)
      beta <- as.greta_array(beta)
      # is_censored <- check_param_greta_array(is_censored)
      # check_numeric_length_1(lower)
      # check_numeric_length_1(upper)
      # check_finite(lower)
      # check_finite(upper)
      # check_x_gte_y(lower, upper)

      # dim <- check_dims(alpha, beta, is_censored, target_dim = dim)

      super$initialize("beta_censored", dim)
      self$add_parameter(alpha, "alpha")
      self$add_parameter(beta, "beta")
      self$add_parameter(is_censored, "is_censored")
      self$censor <- censor
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      alpha <- parameters$alpha
      beta <- parameters$beta
      is_censored <- parameters$is_censored
      beta_dist <- tfp$distributions$Beta(concentration1 = alpha, concentration0 = beta)

      censored_log_prob <- switch(self$censor,
        "right" = function(y) beta_dist$log_survival_function(y),
        "left" = function(y) beta_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- beta_dist$log_cdf(self$upper)
          log_cdf_lower <- beta_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) beta_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) beta_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

#' Normal Censored Distribution
#'
#' Creates a censored normal distribution for use with greta.
#'
#' @param mean Mean of the normal distribution.
#' @param sd Standard deviation of the normal distribution.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `mean`).
#' @return A greta censored normal distribution node.
#' @export
normal_censored <- function(mean, sd, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  if (is.null(tf) || is.null(tfp)) {
    stop(
      "The required Python modules 'tensorflow' and 'tensorflow_probability' are not available. ",
      "Please install them using `tensorflow::install_tensorflow(extra_packages = 'tensorflow-probability')`."
    )
  }
  distrib("normal_censored", mean, sd, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Log-Normal Censored Distribution
#'
#' Creates a censored log-normal distribution for use with greta.
#'
#' @param meanlog Mean of the log-transformed normal distribution.
#' @param sdlog Standard deviation of the log-transformed normal distribution.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `meanlog`).
#' @return A greta censored log-normal distribution node.
#' @export
lognormal_censored <- function(meanlog, sdlog, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  distrib("lognormal_censored", meanlog, sdlog, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Student's t Censored Distribution
#'
#' Creates a censored Student's t distribution for use with greta.
#'
#' @param df Degrees of freedom for the Student's t distribution.
#' @param loc Location parameter (mean).
#' @param scale Scale parameter.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `df`).
#' @return A greta censored Student's t distribution node.
#' @export
student_censored <- function(df, loc, scale, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  if (is.null(tf) || is.null(tfp)) {
    stop(
      "The required Python modules 'tensorflow' and 'tensorflow_probability' are not available. ",
      "Please install them using `tensorflow::install_tensorflow(extra_packages = 'tensorflow-probability')`."
    )
  }
  distrib("student_censored", df, loc, scale, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Gamma Censored Distribution
#'
#' Creates a censored gamma distribution for use with greta.
#'
#' @param shape Shape parameter of the gamma distribution.
#' @param rate Rate parameter of the gamma distribution (reciprocal of scale).
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `shape`).
#' @return A greta censored gamma distribution node.
#' @export
gamma_censored <- function(shape, rate, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  distrib("gamma_censored", shape, rate, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Exponential Censored Distribution
#'
#' Creates a censored exponential distribution for use with greta.
#'
#' @param rate Rate parameter of the exponential distribution.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `rate`).
#' @return A greta censored exponential distribution node.
#' @export
exponential_censored <- function(rate, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  if (is.null(tf) || is.null(tfp)) {
    stop(
      "The required Python modules 'tensorflow' and 'tensorflow_probability' are not available. ",
      "Please install them using `tensorflow::install_tensorflow(extra_packages = 'tensorflow-probability')`."
    )
  }
  distrib("exponential_censored", rate, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Weibull Censored Distribution
#'
#' Creates a censored Weibull distribution for use with greta.
#'
#' @param shape Shape parameter of the Weibull distribution.
#' @param scale Scale parameter of the Weibull distribution.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `shape`).
#' @return A greta censored Weibull distribution node.
#' @export
weibull_censored <- function(shape, scale, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  distrib("weibull_censored", shape, scale, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Pareto Censored Distribution
#'
#' Creates a censored Pareto distribution for use with greta.
#'
#' @param scale Minimum value of the Pareto distribution.
#' @param alpha Shape parameter of the Pareto distribution.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `scale`).
#' @return A greta censored Pareto distribution node.
#' @export
pareto_censored <- function(scale, alpha, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  if (is.null(tf) || is.null(tfp)) {
    stop(
      "The required Python modules 'tensorflow' and 'tensorflow_probability' are not available. ",
      "Please install them using `tensorflow::install_tensorflow(extra_packages = 'tensorflow-probability')`."
    )
  }
  distrib("pareto_censored", scale, alpha, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}

#' Beta Censored Distribution
#'
#' Creates a censored beta distribution for use with greta.
#'
#' @param alpha Shape parameter for successes.
#' @param beta Shape parameter for failures.
#' @param is_censored Logical vector indicating whether each observation is censored.
#' @param censor Type of censoring: one of 'right', 'left', 'interval'.
#' @param lower Lower bound for interval censoring (optional).
#' @param upper Upper bound for interval censoring (optional).
#' @param dim Dimension of the data (optional, defaults to length of `alpha`).
#' @return A greta censored beta distribution node.
#' @export
beta_censored <- function(alpha, beta, is_censored, censor = "right", lower = NULL, upper = NULL, dim = length(is_censored)) {
  if (is.null(tf) || is.null(tfp)) {
    stop(
      "The required Python modules 'tensorflow' and 'tensorflow_probability' are not available. ",
      "Please install them using `tensorflow::install_tensorflow(extra_packages = 'tensorflow-probability')`."
    )
  }
  distrib("beta_censored", alpha, beta, is_censored, censor = censor, lower = lower, upper = upper, dim = dim)
}
