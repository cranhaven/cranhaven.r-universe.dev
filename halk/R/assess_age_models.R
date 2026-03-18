
#' Compute test statistics for comparing actual and estimated ages
#'
#' Using these functions you can compute either a Kolmogorov-Smirnov (KS)
#' statistic or a Chi-squared test statistic to compare estimated ages to actual
#' ages. See details for how each test works and what is reported.
#'
#'
#' The KS test compares length distributions for each age class from known ages
#' against that of estimated ages computed by the \code{\link{assign_ages}}
#' function. The output is a summary value of the test statistics as specified
#' by \code{summary_fun}.
#'
#' The \code{calc_chi_score} function performs a Chi-square test (using the
#' \code{\link[stats]{chisq.test}} function) on the number of estimated and
#' actual ages for each age group.
#'
#' @param data A data.frame containing estimated ages as returned by
#' \code{\link{assign_ages}}
#' @param summary_fun Function used to compute summary statistics for
#' \code{calc_ks_score} for each age group (default is \code{mean})
#' @param age_col Character string specifying the name of the age column
#' @param suppress_warnings Logical. Should any warnings from the function
#' call to \code{ks.test} or \code{chisq.test} be suppressed (TRUE, the default)
#' @param return_val Character. The name of the object to return from the given
#' test
#' @param ... Additional arguments to pass to \code{summary_fun}
#' (\code{calc_ks_score}) or \code{chisq.test} (\code{calc_chi_score})
#'
#' @return A numeric value for each level that was used in the model to assign
#' ages
#' @export
#'
#' @name calc_stat_scores
#'
#' @examples
#' halk <- make_halk(spp_data, levels = c("spp"))
#' newdat <- laa_data
#' newdat$spp <- "bluegill"
#' pred_ages <- assign_ages(newdat, halk)
#' calc_ks_score(pred_ages)
calc_ks_score <- function(data,
                          summary_fun = mean,
                          age_col = "age",
                          suppress_warnings = TRUE,
                          return_val = "statistic",
                          ...) {
  if (!("est.age" %in% names(data))) {
    stop(
      "There must be a column called est.age in data as produced by assign_ages"
    )
  }
  age_levels <- attr(data, "age_levels")
  if (is.null(age_levels)) {
    nested_data <-
      data %>%
      rename_age_col(ac = age_col) %>%
      tidyr::nest(data = tidyselect::everything())
  } else {
    nested_data <-
      data %>%
      rename_age_col(ac = age_col) %>%
      dplyr::group_by(!!!rlang::syms(age_levels)) %>%
      tidyr::nest()
  }
  out <-
    nested_data %>%
    dplyr::summarize(ks.stat = purrr::map_df(data, function(x) {
      out <- lapply(sort(unique(x$age)), function(z) {
        len_at_age <- x$length[x$age == z]
        len_at_est_age <- x$length[x$est.age == z]
        ks <- tryCatch({
          if (suppress_warnings) {
            suppressWarnings(stats::ks.test(len_at_age, len_at_est_age))
          } else {
            stats::ks.test(len_at_age, len_at_est_age)
          }
        }, error = function(e) return(e))
        if ("simpleError" %in% class(ks)) {
          if (ks$message == "not enough 'y' data") {
            ks <- list()
            if (return_val == "statistic") {
              ks <- list(statistic = 1)
            } else if (return_val == "p.value") {
              ks <- list(p.value = 0)
            }
          }
        }
        return(data.frame(age = z, ks.stat = ks[[return_val]]))
      })
      return(out)
    })) %>%
    tidyr::unnest(cols = c(.data$ks.stat)) %>%
    dplyr::ungroup()
  out <-
    out %>%
    dplyr::group_by(!!!rlang::syms(age_levels)) %>%
    dplyr::summarize(ks.sum = summary_fun(.data$ks.stat, ...))
  if (is.null(age_levels)) {
    out <- dplyr::pull(out, .data$ks.sum)
  } else {
    sum_fun_name <- paste0("ks.", as.character(substitute(summary_fun)))
    out <- dplyr::rename(out, !!rlang::sym(sum_fun_name) := .data$ks.sum)
  }
  return(out)
}

#' @rdname calc_stat_scores
#' @export
#' @examples
#' calc_chi_score(pred_ages)
calc_chi_score <- function(data,
                           age_col = "age",
                           suppress_warnings = TRUE,
                           return_val = "statistic",
                           ...) {
  if (!("est.age" %in% names(data))) {
    stop(
      "There must be a column called est.age in data as produced by assign_ages"
    )
  }
  age_levels <- attr(data, "age_levels")
  if (is.null(age_levels)) {
    nested_data <-
      data %>%
      rename_age_col(ac = age_col) %>%
      tidyr::nest(data = tidyselect::everything())
  } else {
    nested_data <-
      data %>%
      rename_age_col(ac = age_col) %>%
      dplyr::group_by(!!!rlang::syms(age_levels)) %>%
      tidyr::nest()
  }
  out <-
    nested_data %>%
    dplyr::summarize(chi_score = purrr::map_dbl(data, function(x) {
      age_table <- dplyr::count(x, .data$age)
      est_age_table <- dplyr::count(x, .data$est.age)
      ages_table <-
        dplyr::full_join(
          age_table, est_age_table, by = c("age" = "est.age")
        ) %>%
        dplyr::mutate(
          n.x = ifelse(is.na(.data$n.x), 0, .data$n.x),
          n.y = ifelse(is.na(.data$n.y), 0, .data$n.y)
        ) %>%
        dplyr::select(-"age") %>%
        as.matrix() %>%
        t()
      if (return_val == "statistic") {
        if (suppress_warnings) {
          chi_test <- suppressWarnings(stats::chisq.test(ages_table, ...))
          return(chi_test$statistic / chi_test$parameter)
        } else {
          chi_test <- chisq.test(ages_table, ...)
          return(chi_test$statistic / chi_test$parameter)
        }
      } else {
        if (suppress_warnings) {
          return(suppressWarnings(stats::chisq.test(ages_table, ...)[[return_val]]))
        } else {
          return(stats::chisq.test(ages_table, ...)[[return_val]])
        }
      }
    }))
  if (is.null(age_levels)) {
    out <- dplyr::pull(out, .data$chi_score)
  }
  return(out)
}

#' Calculate mean-squared-error (MSE) and root mean-squared-error (RMSE) of
#' estimated ages
#'
#' These functions will calculate MSE and RMSE for estimated ages produced by
#' \code{\link{assign_ages}}. Output is specific to each level used by the
#' age-length key to assign ages
#'
#' @param data A data.frame as created by \code{\link{assign_ages}}
#' @param age_col Character. Name of the age column in \code{data}
#'
#' @return Numeric value for estimated ages with no levels or a data.frame with
#' a MSE or RMSE value for each level used to fit ages
#' @export
#'
#' @name calc_mse
#'
#' @examples
#' wae_data <- spp_data[spp_data$spp == "walleye", ]
#' alk <- make_alk(wae_data)
#' wae_est_age <- assign_ages(wae_data, alk)
#' calc_mse(wae_est_age)
calc_mse <- function(data, age_col = "age") {
  return(calc_mse_(data, age_col = age_col))
}

#' @rdname calc_mse
#' @export
#' @examples
#' calc_rmse(wae_est_age)
calc_rmse <- function(data, age_col = "age") {
  return(calc_mse_(data, age_col = age_col, root = TRUE))
}



#' Backend helper function to compute MSE or RMSE
#'
#' This function is the engine for \code{\link{calc_mse}} and
#' \code{\link{calc_rmse}}. It was only created to remove the \code{root}
#' argument from the user in the main \code{calc_mse} function
#'
#' @inheritParams calc_mse
#' @param root Logical. computer MSE (FALSE, default) or RMSE (TRUE)
calc_mse_ <- function(data,
                      age_col = "age",
                      root = FALSE) {
  if (!("est.age" %in% names(data))) {
    stop(
      "There must be a column called est.age in data as produced by assign_ages"
    )
  }
  age_levels <- attr(data, "age_levels")
  mse <- function(obs, pred, rt = root) {
    if (rt) {
      return(sqrt(mean((obs - pred)^2)))
    } else {
      return(mean((obs - pred)^2))
    }
  }
  if (is.null(age_levels)) {
    out <-
      data %>%
      rename_age_col(ac = age_col) %>%
      dplyr::summarize(mse = mse(.data$age, .data$est.age)) %>%
      dplyr::pull()
  } else {
    out <-
      data %>%
      rename_age_col(ac = age_col) %>%
      dplyr::group_by(!!!rlang::syms(age_levels)) %>%
      tidyr::nest() %>%
      dplyr::summarize(mse = purrr::map_dbl(data, function(x) {
        return(mse(x$age, x$est.age))
      })) %>%
      tidyr::unnest(cols = mse) %>%
      dplyr::ungroup()
    if (root) {
      out <- dplyr::rename(out, rmse = mse)
    }
  }
  return(out)
}


#' Compute the quotient of integrals as a measure of percent error between two
#' curves
#'
#' This is a method for comparing how "close" or "accurate" one curve is to
#' another (reference) curve. The method works by dividing the area between the
#' curves by the area under the reference curve. See Details for more
#' information
#'
#' The integral quotient method provides a basis for comparison between two
#' curves by dividing the area between the curves by the area under the
#' reference curve (i.e. the quotient of integrals)
#'
#' @param ref_curve_params A list of named parameters for the reference curve
#'   (i.e. the standard that is being compared to)
#' @param comp_curve_params A list of named parameters for the curve that is
#'   being compared
#' @param min_x The minimum value across which to integrate
#' @param max_x The maximum value across which to integrate
#' @param curve_fun The function that is being compared. Defaults to an
#'   anonymous function that is the von Bertalanffy growth function.
#'
#' @return A value of the area between curves divided by the area under the
#'   reference curve
#' @export
#'
#' @examples
#' ref_curve_params <- list(linf = 60, k = 0.25, t0 = -0.5)
#' comp_curve_params <- list(linf = 62, k = 0.25, t0 = -0.4)
#' comp_curve2_params <- list(linf = 65, k = 0.25, t0 = -1)
#' comp_curve_iq <-
#'  integral_quotient(ref_curve_params, comp_curve_params, 0, 10)
#' comp_curve2_iq <-
#'   integral_quotient(ref_curve_params, comp_curve2_params, 0, 10)
#' vbgf <- function (x, linf, k, t0) {linf * (1 - exp(-k * (x - t0)))}
#' curve(
#'   vbgf(x, ref_curve_params$linf, ref_curve_params$k, ref_curve_params$t0),
#'   from = 0,
#'   to = 10,
#'   ylim = c(0, 60),
#'   xlab = "Age", ylab = "Length"
#' )
#' curve(
#'   vbgf(x, comp_curve_params$linf, comp_curve_params$k, comp_curve_params$t0),
#'   add = TRUE,
#'   col = "blue"
#' )
#' curve(
#'   vbgf(x, comp_curve2_params$linf, comp_curve2_params$k, comp_curve2_params$t0),
#'   add = TRUE,
#'   col = "red"
#' )
#' text(9, 40, labels = paste0(comp_curve_iq, "%"), col = "blue")
#' text(9, 43, labels = paste0(comp_curve2_iq, "%"), col = "red")
integral_quotient <- function(ref_curve_params,
                              comp_curve_params,
                              min_x, max_x,
                              curve_fun = function (x, linf, k, t0) {
                                out <- linf * (1 - exp(-k * (x - t0)))
                                return(out)
                              }) {
  f <- function(x, ref_curve_params, crv_fn) {
    out <- do.call(crv_fn, c(list(x = x), ref_curve_params))
    return(sqrt(out)^2)
  }
  f2 <- function(x, ref_curve_params, comp_curve_params, crv_fn) {
    a <- do.call(crv_fn, c(list(x = x), ref_curve_params))
    b <- do.call(crv_fn, c(list(x = x), comp_curve_params))
    out <- sqrt((a - b)^2)
    return(out)
  }
  ref_curve_auc <- tryCatch(
    stats::integrate(
      f,
      lower = min_x,
      upper = max_x,
      ref_curve_params = ref_curve_params,
      crv_fn = curve_fun
    ),
    error = function(e) return(list(value = NA))
  )
  comp_curve_abc <- tryCatch(
    stats::integrate(
      f2,
      lower = min_x,
      upper = max_x,
      ref_curve_params = ref_curve_params,
      comp_curve_params = comp_curve_params,
      crv_fn = curve_fun
    ),
    error = function(e) return(list(value = NA))
  )
  return(round(comp_curve_abc$value / ref_curve_auc$value, 4) * 100)
}

