#' @title Calculate Observation Level AW-AIPW For Each Treatment Condition
#' @name compute_iaipw
#' @description Calculates the augmented inverse probability weighted estimate (AW-AIPW) of treatment
#' success for each observation and treatment (i.e. on the level of a single unit).
#'
#' @param periods Total periods in the simulation.
#' @param assignment_probs A `tibble`/`data.table` containing the probabilities of being
#' assigned each treatment at a given period.
#' @inheritParams run_mab
#'
#' @returns A named list with the individual aipw estimate vectors for each treatment condition.
#' @details
#' The specification for the individual AW-AIPW estimates can be found
#' in \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)}. The
#' formulas in equation 5, formed the basis for this function's calculations. Here
#' the regression adjustment used is the grouped mean of success by treatment, up until
#' the current period of estimation (so at period 5, the grouped mean would be calculated
#' using the results from periods 1 through 4).
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#' @family estimation
#' @keywords internal
compute_iaipw <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  UseMethod("compute_iaipw", data)
}
#-------------------------------------------------------------------------------
#' @method compute_iaipw data.frame
#' @rdname compute_iaipw
#' @export

compute_iaipw.data.frame <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  mhats <- data |>
    dplyr::group_by(period_number, mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::right_join(
      expand.grid(
        period_number = seq_len(periods),
        mab_condition = conditions,
        stringsAsFactors = FALSE
      ),
      by = c("period_number", "mab_condition")
    ) |>
    dplyr::mutate(dplyr::across(c(successes, n), \(x) {
      tidyr::replace_na(x, 0)
    })) |>
    dplyr::arrange(mab_condition, period_number) |>
    dplyr::group_by(mab_condition) |>
    dplyr::mutate(
      mhat = dplyr::lag(
        dplyr::if_else(cumsum(n) > 0, cumsum(successes) / cumsum(n), 0),
        n = 1L,
        default = 0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(period_number, mab_condition, mhat) |>
    tidyr::pivot_wider(names_from = mab_condition, values_from = mhat) |>
    dplyr::arrange(period_number)
  periods_vec <- data[["period_number"]]
  conditions_vec <- data[["mab_condition"]]
  success_vec <- data[["mab_success"]]

  iaipw_estimates <- lapply(
    conditions,
    \(condition) {
      prob <- assignment_probs[[condition]][periods_vec]
      mhat <- mhats[[condition]][periods_vec]
      return(
        iaipw(
          conditions_vec = conditions_vec,
          success_vec = success_vec,
          mhat = mhat,
          prob = prob,
          condition = condition
        )
      )
    }
  )
  names(iaipw_estimates) <- conditions

  check <- vapply(iaipw_estimates, \(x) sum(is.na(x)), numeric(1)) |> sum()

  if (check != 0) {
    warning(paste0(check, " Individual AIPW Scores are NA"))
  }
  return(iaipw_estimates)
}
# ------------------------------------------------------------------------------
#' @method compute_iaipw data.table
#' @export
#' @rdname compute_iaipw

compute_iaipw.data.table <- function(
  data,
  assignment_probs,
  periods,
  conditions
) {
  mhats <- data[,
    .(
      successes = sum(mab_success, na.rm = TRUE),
      n = .N
    ),
    by = .(mab_condition, period_number)
  ] |>
    merge(
      data.table::CJ(
        period_number = seq_len(periods),
        mab_condition = conditions
      ),
      by = c("period_number", "mab_condition"),
      all.y = TRUE
    )

  mhats[, `:=`(
    successes = data.table::fifelse(is.na(successes), 0, successes),
    n = data.table::fifelse(is.na(n), 0, n)
  )]
  data.table::setorder(mhats, mab_condition, period_number)

  mhats <- mhats[,
    mhat := data.table::shift(
      data.table::fifelse(cumsum(n) > 0, cumsum(successes) / cumsum(n), 0),
      n = 1L,
      fill = 0,
      type = "lag"
    ),
    by = mab_condition
  ][, .(period_number, mab_condition, mhat)] |>
    data.table::dcast(
      formula = period_number ~ mab_condition,
      value.var = "mhat"
    )

  data.table::setorder(mhats, period_number)

  periods_vec <- data[["period_number"]]
  conditions_vec <- data[["mab_condition"]]
  success_vec <- data[["mab_success"]]

  iaipw_estimates <- lapply(
    conditions,
    \(condition) {
      prob <- assignment_probs[periods_vec, ..condition][[1]]
      mhat <- mhats[periods_vec, ..condition][[1]]
      return(
        iaipw(
          conditions_vec = conditions_vec,
          success_vec = success_vec,
          mhat = mhat,
          prob = prob,
          condition = condition
        )
      )
    }
  )
  names(iaipw_estimates) <- conditions

  check <- vapply(iaipw_estimates, \(x) sum(is.na(x)), numeric(1)) |> sum()

  if (check != 0) {
    warning(paste0(check, " Individual AIPW Scores are NA"))
  }

  return(iaipw_estimates)
}

#' @describeIn compute_iaipw Small function to handle iaipw computation in both branches.
#' @keywords internal

iaipw <- function(conditions_vec, success_vec, mhat, prob, condition) {
  # If prob is 0, indicator is NaN or Inf
  indicator <- (as.integer(conditions_vec == condition) / prob)
  indicator[!is.finite(indicator)] <- 0
  iaipw <- (indicator * success_vec) + (1 - indicator) * mhat
  return(iaipw)
}

#-------------------------------------------------------------------------------
#' Calculate Adaptive AW-AIPW Estimates
#' @name estimate_aw_aipw
#'
#' @description Uses provided Individual AIPW scores created by [compute_iaipw()] and computes the final
#' Adaptively Weighted AIPW estimate and variance using the formulas from
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et. al (2021)}.
#' Uses the constant allocation rate adaptive weight.
#'
#' @inheritParams compute_iaipw
#' @inheritParams run_mab
#' @param iaipw Individual AW-AIPW scores computed by [compute_iaipw()].
#' @param cluster_col String; name of column with cluster indices.
#' @returns A `tibble`/`data.table` containing 5 columns:
#' \itemize{
#'   \item `means`: AW-AIPW statistics.
#'   \item `se`: AW-AIPW standard errors.
#'   \item `mab_condition`: Treatment arm.
#'   \item `estimator`: "AW-AIPW".
#'   \item `df`: Degrees of freedom, `NA` implies normal distribution Z test.
#' }
#' @details
#' The formulas for the calculations in this function can be found in
#' \href{https://www.pnas.org/doi/full/10.1073/pnas.2014602118}{Hadad et al. (2021)} at
#' equation 5 (estimate), equation 11 (variance), equation 15 (allocation rate).
#'
#' The estimator assumes pure sequential assignment, but we adapt the estimator to our batched assignment procedure.
#' In the computations, of the individual estimates, regression estimates were only computed for each period,
#' instead of for each observation, and similarly the adaptive weights used will only be computed per period, and then
#' simply assigned to all the observations in that period, thus resulting in only a few unique weights. This
#' keeps effective sample size large, ensuring the asymptotic properties are realized in large samples with only
#' a few assignment periods, while also properly accounting for the assignment procedure.
#'
#' If clustering is specified, within each period individual AIPW estimates are aggregated by cluster,
#' and then the sample size becomes the sum of the number of clusters in each period,
#' the variance formula uses the cluster deviations instead (an effective CR0 style estimator),
#' which is adjusted via the Stata CR1 estimator (\eqn{\frac{G}{G-1} * \frac{N-1}{N-k}}) where k is
#' the number of treatments, and G is the number of clusters.
#'
#' The AW-AIPW estimator is unbiased, consistent, and asymptotically normal when a sufficently
#' large non-zero
#' probability of assignment is guaranteed for each treatment condition in each period. The provided standard errors
#' of the so can be used for valid inference with a normal distribution. Treatment effects can also be estimated
#' as the difference in AW-AIPW estimates with the variance of the difference as the sum of the
#' variances of the two arms. In the clustered case, we suggest a t distribution to be more
#' conservative, given the sample size is now the cluster, we provide \eqn{G-1} as degrees of
#' freedom, as standard by regression packages for clustered inference (CR0, and CR1).
#'
#' @references
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021.
#' "Confidence Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National Academy of Sciences of the United States of America} 118
#' (15): e2014602118. \doi{10.1073/pnas.2014602118}.
#'
#' @family estimation
#' @keywords internal
estimate_aw_aipw <- function(
  data,
  assignment_probs,
  conditions,
  iaipw,
  cluster_col,
  clustering,
  periods,
  num_clusters = NULL
) {
  dt <- data.table::is.data.table(data)
  iaipw_periods <- if (clustering) {
    if (dt) {
      iaipw_scores <- data.table::cbindlist(
        list(data, data.table::as.data.table(iaipw))
      )[,
        lapply(.SD, \(x) mean(x, na.rm = TRUE)),
        .SDcols = conditions,
        by = c("period_number", cluster_col)
      ] |>
        as.list()
    } else {
      iaipw_scores <- cbind(data, tibble::as_tibble(iaipw)) |>
        dplyr::group_by(period_number, .data[[cluster_col]]) |>
        dplyr::summarize(dplyr::across(
          dplyr::all_of(unname(conditions)),
          \(x) mean(x, na.rm = TRUE)
        )) |>
        as.list()
    }
    list(
      iaipw = iaipw_scores[conditions],
      period_numbers = iaipw_scores[["period_number"]]
    )
  } else {
    list(iaipw = iaipw, period_numbers = data[["period_number"]])
  }

  bind_func <- if (dt) data.table::rbindlist else dplyr::bind_rows

  aw_aipw_estimates <- purrr::imap(
    iaipw_periods[["iaipw"]],
    \(score, name) {
      weights <- sqrt(assignment_probs[[name]] / length(score))[iaipw_periods[[
        "period_numbers"
      ]]]
      sum_w <- sum(weights)
      mean <- sum(score * weights) / sum_w
      var <- (sum((weights^2) * ((score - mean)^2)) / ((sum_w)^2))
      if (clustering) {
        se <- sqrt(cr1(
          var,
          g = num_clusters,
          n = nrow(data),
          k = length(conditions)
        ))
        df <- num_clusters - 1
      } else {
        se <- sqrt(var)
        df <- NA
      }
      return(
        list(
          mean = mean,
          se = se,
          mab_condition = name,
          estimator = "AW-AIPW",
          df = df
        )
      )
    }
  ) |>
    bind_func()
  return(aw_aipw_estimates)
}

#' OLS Estimates for Probability of Success
#' @name estimate_lm
#' @description
#' Computes OLS estimates for true true probabilities of success using [estimatr::lm_robust()].
#' Supports IPW weighted and unweighted regression. If clustering is specified CR2 standard errors are reported. Otherwise HC2
#' standard errors are used. Appropriate degrees of freedom are supplied along with the regression's F-statistic
#'
#' @inheritParams compute_iaipw
#' @inheritParams run_mab
#' @inheritParams estimate_aw_aipw
#' @param ipw Logical. If `TRUE` IPW-weighted LPM; if
#'   `FALSE`, fits the unweighted OLS LPM.
#' @details
#'
#' If CR2 standard errors fail to be calculated, CR1S are computed, equivalent to `vce(cluster,
#' clustervar)` in Stata, and `se_type = "stata"` in `{estimatr}`
#'
#' These estimates follow the procedure in
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)}.
#' Degrees of freedom are not provided for the f-statistic, because the traditional F-distribution is invalid
#' under the adaptive procedure. However, this f-statistic can be used for the randomization and
#' bootstrap inference joint-tests provided.
#'
#' The provided standard errors can be used to construct approximate confidence intervals using a t-distribution with
#' the provided degrees of freedom. However the degrees of freedom provided are `n - num_conditions`,
#' which is likely to be an overestimate given the potential for the number of observations assigned
#' to each group to vary widely with the adaptive trial. The HC2 or CR2 corrections help but
#' do not capture the temporal dependence created by adaptive assignment. Thus formal pairwise tests
#' cannot be conducted using these estimates, because the estimator does not follow a precise t-distribution.
#'
#' As mentioned in \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)},
#' if there is a true best arm, and control augmentation is used, t-test can provide proper coverage
#' in large samples because the algorithm converges to a two-arm comparison.
#'
#' Block fixed effects are not used for estimation due to the prevalence of numerical instability
#' in the estimates. Assignment probabilities to treatment are the same within each block,
#' so the IPW estimator is still unbiased without the presence of the fixed effects.
#'
#' @returns A list containing 3 elements:
#' \itemize{
#'   \item `estimates`: data.table/data.frame of coefficients, standard errors, and degrees of freedom.
#'   \item `f_stat`: F statistic from regression
#'   \item `model`: `lm_robust` object, only saved under clustering.
#' }
#' @family estimation
#' @keywords internal

#' @references
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#' \emph{American Journal of Political Science} 65 (4): 826–44. \doi{10.1111/ajps.12597}.
#'
estimate_lm <- function(
  data,
  cluster_col,
  clustering,
  conditions,
  num_clusters = NULL,
  ipw
) {
  lm_fun <- purrr::partial(
    estimatr::lm_robust,
    formula = mab_success ~ mab_condition - 1,
    data = data
  )

  if (ipw) {
    lm_fun <- purrr::partial(lm_fun, weights = data[["ipw_weights"]])
  }

  if (clustering) {
    est_lm <- tryCatch(
      {
        x <- lm_fun(
          clusters = data[[cluster_col]],
          se_type = "CR2"
        )
      },
      error = function(e) {
        rlang::warn("CR2 failed. Falling back to Stata CR1")
        x <- lm_fun(
          clusters = data[[cluster_col]],
          se_type = "stata"
        )
        return(x)
      }
    )
  } else {
    est_lm <- lm_fun(se_type = "HC2")
  }

  items <- lapply(c("coefficients", "std.error", "df"), \(x) {
    strip_condition_prefix(est_lm[[x]])
  })

  estimator <- if (ipw) "IPW" else "OLS"
  df_func <- if (data.table::is.data.table(data)) {
    data.table::data.table
  } else {
    tibble::tibble
  }

  lm_estimates <- df_func(
    mean = items[[1]],
    se = items[[2]],
    df = items[[3]],
    mab_condition = names(items[[1]]),
    estimator = estimator
  )
  model <- if (clustering) est_lm else NULL
  return(list(
    estimates = lm_estimates,
    f_stat = as.numeric(est_lm[["fstatistic"]][1]),
    model = model
  ))
}

#' Helper Functions for Inference
#' @name inference_helpers
#' @description Internal helpers for estimation in [run_mab()].
#' @keywords internal
NULL


#' Strip the `mab_condition` Prefix from Model Names
#' @describeIn inference_helpers Strips coefficient name prefixes from named vector or matrix's dimnames.
#' @param x A named vector, or a square matrix with matching dimnames.
#' @returns `x` with the prefix removed.
#' @keywords internal
strip_condition_prefix <- function(x) {
  clean <- \(nm) gsub("^mab_condition", "", nm)
  if (is.matrix(x)) {
    dimnames(x) <- lapply(dimnames(x), clean)
  } else {
    names(x) <- clean(names(x))
  }
  return(x)
}


#' Fill Missing Conditions
#' @description
#' Accepts a `data.frame` like object, and a character of vector of `conditions`. It checks
#' whether or not all provided conditions are present in the data, if not their values are initialized to NA
#' @param x A `tibble`/`data.table` containing the appropriate estimates
#' @param conditions Character vector of treatment condition labels.
#' @returns An updated `estimates` object with missing conditions initialized.
#' @rdname inference_helpers
#' @family estimation
fill_missing_conditions <- function(x, conditions, estimator) {
  UseMethod("fill_missing_conditions", x)
}

#' @rdname inference_helpers
#' @method fill_missing_conditions data.frame
#' @export
fill_missing_conditions.data.frame <- function(x, conditions, estimator) {
  missing_conditions <- setdiff(conditions, x[["mab_condition"]])

  if (length(missing_conditions) > 0) {
    x <- dplyr::bind_rows(
      x,
      tibble::tibble(
        mean = NA,
        se = NA,
        mab_condition = missing_conditions,
        estimator = estimator
      )
    )
  }

  return(x)
}

#' @rdname inference_helpers
#' @method fill_missing_conditions data.table
#' @export
fill_missing_conditions.data.table <- function(x, conditions, estimator) {
  missing_conditions <- setdiff(conditions, x[["mab_condition"]])

  if (length(missing_conditions) > 0) {
    x <- data.table::rbindlist(
      list(
        x,
        data.table::data.table(
          mean = NA,
          se = NA,
          mab_condition = missing_conditions,
          estimator = estimator
        )
      ),
      fill = TRUE
    )
  }

  return(x)
}
#' Combine Estimates
#' @description
#' Combines the AW-AIPW, IPW, and Sample estimates into a single object to be returned.
#' @param ... `tibbles` or `data.tables` to bind together.
#' @returns A list of 2 elements:
#' \itemize{
#' \item `estimates`: Input `estimates` bound together by rows.
#' \item `vcov`: `vcov` input
#' }
#' @family estimation
#' @rdname inference_helpers
combine_estimates <- function(...) {
  tbls <- rlang::dots_list(...)
  est <- if (data.table::is.data.table(tbls[[1]])) {
    data.table::rbindlist(tbls, fill = TRUE)
  } else {
    dplyr::bind_rows(tbls)
  }
  if (nrow(est) == 0) {
    return(NULL)
  }
  return(est)
}

#' CR1 Adjustment
#' @description
#' Performs adjustment of CR0 SE to CR1 SE, using Stata's formula
#' @param x vector of variances.
#' @param g integer number of clusters
#' @param k integer number of treatments
#' @param n integer, dataset size
#' @returns An adjusted vector of variances
#' @family estimation
#' @rdname inference_helpers
#'
cr1 <- function(x, g, n, k) {
  x * (g / (g - 1)) * ((n - 1) / (n - k))
}
