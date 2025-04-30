#' Aberration calculation functions
#'
#' @param data Count or case data.
#' @param aberr_prefix Prefix of the aberrations in the data.
#' @param power Power of aberration.
#' @param X Sum of detected aberrations.
#' @param X2 Quadratic sum of detected aberrations.
#' @param N Number of cells analysed.
#' @param mean Mean.
#' @param var Variance.
#' @param assessment_u Expected \eqn{u}-value of the assessment. For a Poisson distribution this should be unity.
#' @param type Type of input data. Either "count" and "case".
#' @param aberr_module Aberration module.
#' @name calculate_aberr
NULL
# > NULL

#' @rdname calculate_aberr
calculate_aberr_power <- function(data, aberr_prefix = "C", power = 1) {
  # Prepare data
  aberr_data <- data %>%
    .[grep(aberr_prefix, names(.))] %>%
    t() %>%
    as.data.frame()

  powers <- aberr_data %>%
    rownames() %>%
    regmatches(., regexpr("[0-9]+", .)) %>%
    as.numeric() %>%
    `^`(power)

  # Calculate aberration powers
  aberr <- numeric(length = ncol(aberr_data))

  for (i in seq_len(ncol(aberr_data))) {
    aberr[i] <- sum(aberr_data[, i] * powers)
  }

  return(aberr)
}

#' @rdname calculate_aberr
calculate_aberr_mean <- function(X, N) {
  mean <- X / N

  return(mean)
}

#' @rdname calculate_aberr
calculate_aberr_var <- function(X, X2, N) {
  var <- (X2 - X^2 / N) / (N - 1)

  return(var)
}

#' @rdname calculate_aberr
calculate_aberr_disp_index <- function(mean, var) {
  disp_index <- var / mean

  return(disp_index)
}

#' @rdname calculate_aberr
calculate_aberr_u_value <- function(X, N, mean, var, assessment_u = 1) {
  u_value <- (var / mean - assessment_u) * sqrt((N - 1) / (2 * (1 - 1 / X)))

  return(u_value)
}

#' @rdname calculate_aberr
#' @importFrom rlang .data
init_aberr_table <- function(data, type = c("count", "case"), aberr_module = c("dicentrics", "translocations", "micronuclei")) {
  # Validate parameters
  type <- match.arg(type)
  aberr_module <- match.arg(aberr_module)

  if (type == "count") {
    data <- data %>%
      dplyr::mutate(
        N = 0,
        X = 0,
        mean = 0,
        var = 0,
        DI = 0,
        u = 0
      ) %>%
      dplyr::select("D", "N", "X", dplyr::everything()) %>%
      dplyr::mutate(
        D = as.numeric(.data$D)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c("N", "X", grep("C", names(.), value = TRUE)),
          .fns = as.integer
        )
      )
  } else if (type == "case") {
    if (aberr_module %in% c("dicentrics", "micronuclei")) {
      data <- data %>%
        dplyr::mutate(
          N = 0,
          X = 0,
          y = 0,
          y_err = 0,
          DI = 0,
          u = 0
        ) %>%
        dplyr::select("N", "X", dplyr::everything()) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = c("N", "X", grep("C", names(.), value = TRUE)),
            .fns = as.integer
          )
        )
    } else if (aberr_module == "translocations") {
      data <- data %>%
        dplyr::mutate(
          N = 0,
          X = 0,
          Fp = 0,
          Fp_err = 0,
          DI = 0,
          u = 0,
          Xc = 0,
          Fg = 0,
          Fg_err = 0
        ) %>%
        dplyr::select("N", "X", dplyr::everything()) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = c("N", "X", grep("C", names(.), value = TRUE)),
            .fns = as.integer
          )
        )
    }
  }

  return(data)
}

#' Calculate aberrations table
#'
#' @param data Count or case data.
#' @param type Type of input data. Either "count" and "case".
#' @param assessment_u Expected \eqn{u}-value of the assessment. For a Poisson distribution this should be unity.
#' @param aberr_module Aberration module, required for \code{type = "case"}.
#'
#' @return Data frame containing cell count (\eqn{N}), aberrations (\eqn{X}),
#' and other coefficients (dispersion index, \eqn{u}-value, ...), as well as
#' raw count or case \code{data}.
#' @export
#' @importFrom rlang .data
calculate_aberr_table <- function(data, type = c("count", "case"), aberr_module = c("dicentrics", "translocations", "micronuclei"), assessment_u = 1) {
  # Validate parameters
  type <- match.arg(type)
  aberr_module <- match.arg(aberr_module)

  if (type == "count") {
    data <- data %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        N = as.integer(rowSums(.[grep("C", names(.))])),
        X = calculate_aberr_power(., power = 1),
        X2 = calculate_aberr_power(., power = 2),
        mean = calculate_aberr_mean(.data$X, .data$N),
        var = calculate_aberr_var(.data$X, .data$X2, .data$N),
        DI = calculate_aberr_disp_index(.data$mean, .data$var),
        u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = assessment_u)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(grep("C", names(.), value = TRUE)),
          .fns = as.integer
        )
      ) %>%
      dplyr::select(-"X2") %>%
      dplyr::select("D", "N", "X", dplyr::everything())
  } else if (type == "case") {
    data <- data %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        N = as.integer(rowSums(.[grep("C", names(.))])),
        X = calculate_aberr_power(., power = 1),
        X2 = calculate_aberr_power(., power = 2),
        var = calculate_aberr_var(.data$X, .data$X2, .data$N),
        mean = calculate_aberr_mean(.data$X, .data$N),
        std_err = sqrt(.data$var / .data$N),
        DI = calculate_aberr_disp_index(.data$mean, .data$var),
        u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = assessment_u)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c("N", "X", grep("C", names(.), value = TRUE)),
          .fns = as.integer
        )
      ) %>%
      dplyr::select(-"X2", -"var") %>%
      dplyr::select("N", "X", dplyr::everything())

    # Rename mean and std_err columns
    if (aberr_module %in% c("dicentrics", "micronuclei")) {
      data <- data %>%
        dplyr::select(
          "N", "X", dplyr::matches("^C[0-9]+$"),
          "y" = "mean", "y_err" = "std_err",
          "DI", "u"
        )
    } else if (aberr_module == "translocations") {
      data <- data %>%
        dplyr::select(
          "N", "X", dplyr::matches("^C[0-9]+$"),
          "Fp" = "mean", "Fp_err" = "std_err",
          "DI", "u"
        )
    }
  }

  return(data)
}
