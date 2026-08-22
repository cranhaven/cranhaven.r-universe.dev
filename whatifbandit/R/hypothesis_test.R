#' @title Joint Hypothesis Test for Multi-Arm Bandit Trials
#' @name joint_test
#' @description Conducts a joint hypothesis test of no treatment effects across all arms, i.e. that all arms
#' have the same true probability of success, either using a bootstrap procedure or the randomization inference
#' procedure adapted from
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)}.
#' See details for a description of both methods
#' @param mab A `single_rct_mab` or `single_param_mab` object.
#' @param method A character string; either `"bootstrap"` or `"randomization"`.
#' @param r A positive integer; number of simulations used to build the null distribution.
#' Default is 100.
#'
#' @return A named list object containing
#' \itemize{
#'   \item `f_statistic`: The observed F-statistic from the IPW regression.
#'   \item `null_distribution`: A numeric vector of F-statistics under the null.
#'   \item `p_value`: The proportion of simulated F-statistics more extreme than observed.
#'   \item `method`: The method used.
#'   \item `r`: Number of replications used.
#'   \item `effective_r`: Number of generated f_stats which are not `NULL` or `NA`.
#' }
#' @export
#' @details
#' # NOTE
#' This procedure is experimental and has no Type I error guarantee. Offer-Westort et. al (2021)
#' also note the test suffers from low power, but it is provided for experimentation nonetheless.
#'
#' `method = "randomization"` operates under the sharp null that each unit
#' would express the same outcome no matter the treatment they were assigned. To achieve this
#' the trial is re-simulated but new outcomes are not generated or imputed, however the adaptive algorithm
#' still changes the assignments. This results in a null distribution that captures how the adaptive
#' algorithm will assign even when the outcomes are not related to treatments at all.
#'
#' `method = "bootstrap"` operates under the null hypothesis that there is no difference between
#' treatment arms within each block/cluster. If there are no blocks or clusters, a
#' p-matrix is built from the pooled sample mean of the original trial. With block and or cluster
#' pooled sample means are estimated within each block or cluster. The block and or cluster assignment
#' proportions are taken from the original dataset.
#'
#'
#'
#' @references
#' Offer-Westort, Molly, Alexander Coppock, and Donald P. Green.
#' "Adaptive Experimental Design: Prospects and Applications in Political Science."
#'  American Journal of Political Science 65, no. 4 (2021): 826–44. \doi{10.1111/ajps.12597}.
#' @examples
#'
#' data(tanf)
#' set.seed(5)
#' adaptive <- mab_from_rct(success ~ condition, data = tanf, algorithm = "thompson",
#' period_method = "batch", period_length = 500)
#'
#' # Low `r` for examples, use replications in practice
#' joint_test(adaptive, "randomization", r = 2)
#' joint_test(adaptive, "bootstrap", r = 2)
#'
#'
joint_test <- function(
  mab,
  method = c("bootstrap", "randomization"),
  r = 100
) {
  check_posint(r)
  method <- rlang::arg_match(method)
  if (!inherits(mab, "single_mab")) {
    rlang::abort(c("Joint-tests can only be performed on `single_mab` objects"))
  }
  if (method == "randomization" && inherits(mab, "single_rct_mab")) {
    rlang::warn(c(
      "Randomization inference may not be informative for resimulated RCT objects."
    ))
  }

  null <- switch(
    method,
    "bootstrap" = joint_boot_null(mab = mab, r = r),
    "randomization" = joint_random_null(mab = mab, r = r),
  )
  f <- mab$f_stats[["IPW"]]

  null <- null[!is.na(null) & is.finite(null)]
  if (length(null) != r) {
    rlang::warn(c(paste(
      "Test produced ",
      r - length(null),
      "NA F-statistics. "
    )))
  }

  p <- mean(null > f)

  return(list(
    f_stat = f,
    null_distribution = null,
    p_value = p,
    method = method,
    r = r,
    effective_r = length(null)
  ))
}

#' Helpers for Joint F Test
#' @name f_helpers
#' @description Takes the `single_mab` object provided and returns the proper
#' null distribution for the randomization or the bootstrap joint test.
#' @returns a numeric vector of simulated F-statistics
NULL

#' @describeIn f_helpers Prepares arguments for the randomization joint test.
#' @inheritParams joint_test
#' @keywords internal
joint_random_null <- function(mab, r) {
  args <- joint_base_args(mab, sim_type = "test")

  na_rows <- args$period_idxs$start_idxs[2]:nrow(args$data)
  cols_to_drop <- c("mab_assign_prop", "ipw_weights")
  for (col in cols_to_drop) {
    if (col %in% names(args$data)) {
      if (data.table::is.data.table(args$data)) {
        args$data[, (col) := NULL]
      } else {
        args$data[, col] <- NULL
      }
    }
  }

  furrr::future_map_dbl(
    seq_len(r),
    \(.) joint_null_inner(args),
    .options = mab$config$parallel,
    .progress = mab$config$args$verbose
  )
}

#' @describeIn f_helpers Prepares arguments for the parametric bootstrap joint test.
#' @inheritParams joint_test
#' @keywords internal
joint_boot_null <- function(mab, r) {
  args <- joint_base_args(mab, sim_type = "param")

  if (inherits(mab, "single_rct_mab")) {
    col_names <- args$col_names

    time_model_args <- build_time_model_args(mab, args, col_names)

    args <- utils::modifyList(
      args,
      list(
        assignment_dates = if (is.null(col_names$assignment_date_col)) {
          NULL
        } else {
          mab$new_data[[col_names$assignment_date_col]]
        },
        blocks = if (is.null(mab$config$args$blocks)) {
          NULL
        } else {
          group_prop(mab$new_data, "block")
        },
        clusters = build_rct_clusters(mab, col_names),
        n = nrow(mab$new_data),
        dt = data.table::is.data.table(mab$new_data),
        equal_probs = rep(1 / length(args$conditions), length(args$conditions)),
        simulate_dates = mab$config$args$delayed_feedback,
        col_names = list(
          cluster_col = "cluster",
          assignment_date_col = "assignment_date",
          success_date_col = "success_date"
        ),
        time_model = time_model_args$time_model,
        time_model_args = time_model_args$args,
        whole_experiment = NULL,
        data = NULL
      )
    )

    success_col <- col_names$success_col
    group_col <- if (!is.null(args$clusters)) {
      col_names$cluster_col
    } else if (!is.null(args$blocks)) {
      "block"
    } else {
      NULL
    }
    dn <- list(mab$config$args$conditions, sort(names(build_p_cols(args))))
  } else {
    success_col <- "mab_success"
    group_col <- if (!is.null(args$clusters)) {
      "cluster"
    } else if (!is.null(args$blocks)) {
      "block"
    } else {
      NULL
    }
    dn <- dimnames(mab$config$args$p) |> lapply(sort)
  }

  build_p <- boot_build_p(
    data = mab$new_data,
    success_col = success_col,
    group_col = group_col,
    cols = build_p_cols(args)
  )
  args[["p"]] <- matrix(
    build_p$s / build_p$n,
    nrow = length(mab$config$args$conditions),
    ncol = length(build_p$cols),
    byrow = TRUE,
    dimnames = dn
  )
  null <- furrr::future_map_dbl(
    seq_len(r),
    \(.) {
      joint_null_inner(args = args)
    },
    .options = mab$config$parallel,
    .progress = mab$config$args$verbose
  )
}

#' Build Proper Arguments for RCT Bootstrap Joint Test
#' @name build_rct
#' @keywords internal
#'
NULL

#' @describeIn build_rct Resolves the `clusters` argument for a `single_rct_mab` bootstrap.
#' When both blocks and clusters are present, clusters is a named list of
#' per-block cluster proportion vectors (as documented in [simulate_mab()]).
#' When only clusters are present, returns a flat named proportion vector via
#' [group_prop()]. Returns `NULL` when no clustering was used.
#' @param mab A `single_rct_mab` object.
#' @param col_names Named list of column name strings from `args$col_names`.
#' @returns A named numeric vector, named list of vectors, or `NULL`.
#' @keywords internal
build_rct_clusters <- function(mab, col_names) {
  if (is.null(mab$config$args$clusters)) {
    return(NULL)
  }
  if (!is.null(mab$config$args$blocks)) {
    data <- mab$new_data
    blocks <- unique(
      if (data.table::is.data.table(data)) data[["block"]] else data$block
    )
    lapply(
      stats::setNames(blocks, blocks),
      \(b) {
        block_data <- if (data.table::is.data.table(data)) {
          data[block == b]
        } else {
          data[data$block == b, ]
        }
        group_prop(block_data, col_names$cluster_col)
      }
    )
  } else {
    group_prop(mab$new_data, col_names$cluster_col)
  }
}

#' @describeIn build_rct Returns the named vector of proportions that drives the columns of the null
#' `p` matrix — clusters if present, blocks if present, or a scalar `1` for
#' the no-blocking/no-clustering case. For the blocked-and-clustered case,
#' clusters is a named list; this flattens it to a single named vector (since
#' the p matrix columns are individual clusters, not blocks).
#' @param args The processed args list from [joint_base_args()] (or after
#'   [utils::modifyList()]) which contains `$clusters` and `$blocks`.
#' @returns A named numeric vector of proportions, or a scalar `1`.
#' @keywords internal
build_p_cols <- function(args) {
  if (!is.null(args$clusters)) {
    if (is.list(args$clusters)) {
      unlist(unname(args$clusters))
    } else {
      args$clusters
    }
  } else if (!is.null(args$blocks)) {
    args$blocks
  } else {
    c(`1` = 1)
  }
}


#' @describeIn build_rct Recovers successes and totals for each column group (cluster, block, or
#' the whole dataset), then returns them alongside the resolved column
#' proportions vector for use in [joint_boot_null()].
#' @param data Input data.
#' @param success_col Name of the success column.
#' @param group_col Column to group by, or `NULL` for the whole dataset.
#' @param cols Named numeric vector of column proportions (from [build_p_cols()]).
#' @returns A list with elements `cols`, `s` (successes), and `n` (totals),
#'   all named and sorted consistently.
#' @keywords internal
boot_build_p <- function(data, success_col, group_col, cols) {
  counts <- boot_null_counts(data, success_col, group_col)
  if (!is.null(group_col)) {
    s <- as_named_vec(counts, val = "s", name = group_col)
    n <- as_named_vec(counts, val = "n", name = group_col)
    list(cols = cols, s = s[order(names(s))], n = n[order(names(n))])
  } else {
    list(cols = cols, s = counts$s, n = counts$n)
  }
}

#' @describeIn build_rct Constructs the `time_model` function and its associated argument list when
#' `delayed_feedback` is enabled for a `single_rct_mab` bootstrap. Returns a
#' list with `time_model = NULL` and `args = list()` when delayed feedback is
#' not in use.
#' @param mab A `single_rct_mab` object.
#' @param args The processed args list from [joint_base_args()].
#' @param col_names Named list of column name strings.
#' @returns A list with elements `time_model` (function or `NULL`) and `args`
#'   (list of additional arguments for the time model).
#' @keywords internal
build_time_model_args <- function(mab, args, col_names) {
  if (!args$delayed_feedback) {
    return(list(
      time_model = NULL,
      args = list(impute_dates = NULL, original_dates = NULL)
    ))
  }

  impute_dates <- precompute_imputation(
    data = mab$new_data,
    whole_experiment = TRUE,
    delayed_feedback = args$delayed_feedback,
    col_names = col_names
  )[["dates"]]

  original_dates <- if (data.table::is.data.table(mab$new_data)) {
    mab$new_data[,
      .SD,
      .SDcols = c("period_number", col_names$assignment_date_col),
    ] |>
      split(by = "period_number") |>
      lapply(\(x) x[[col_names$assignment_date_col]])
  } else {
    mab$new_data |>
      dplyr::select(dplyr::all_of(c(
        "period_number",
        col_names$assignment_date_col
      ))) |>
      dplyr::group_split(period_number) |>
      lapply(\(x) x[[col_names$assignment_date_col]])
  }

  time_model <- function(
    n,
    conditions,
    successes,
    current_period,
    blocks = NULL,
    clusters = NULL,
    impute_dates,
    original_dates
  ) {
    treatment_block <- paste(conditions, successes, sep = "_")
    dates <- impute_dates[[current_period]][treatment_block]
    org <- original_dates[[current_period]]
    return(dates - org)
  }

  list(
    time_model = time_model,
    args = list(impute_dates = impute_dates, original_dates = original_dates)
  )
}

#' @describeIn f_helpers Extracts common arguments from [run_mab_single()] and the
#' `single_mab` provided object's arguments slot.
#' @returns A named list of arguments to be used for [run_mab_single()]
#' @inheritParams joint_test
#' @inheritParams run_mab
#' @keywords internal
joint_base_args <- function(mab, sim_type) {
  args <- mab$config$args[intersect(
    names(mab$config$args),
    methods::formalArgs(run_mab_single)
  )] |>
    utils::modifyList(
      list(
        sim_type = sim_type,
        blocking = !is.null(mab$config$args$blocks),
        clustering = !is.null(mab$config$args$clusters),
        estimators = "ipw",
        contrasts = NULL,
        keep_models = FALSE
      )
    )
  if (sim_type == "param") {
    args$p <- NULL
  }
  if (sim_type == "test") {
    args$data <- mab$new_data
  }
  return(args)
}

#' @describeIn f_helpers inner function for [furrr::future_map()]
#' @param args Arguments list to [run_mab_single()]
#' @returns The F-statistic from the IPW regression of the MAB Trial
#' @keywords internal
joint_null_inner <- function(args) {
  do.call(run_mab_single, args)[["f_stats"]][["IPW"]]
}

#' Get Group Proportions
#' @description Accepts input data, and a group column, and returns the proportion of the data that belongs
#' to each group
#'
#' @param data Input Data
#' @param group Column to group by
#'
#' @returns A named numeric vector with the `names` corresponding to the group, and the value
#' to its proportion among the provided data.
#' @keywords internal
#'
group_prop <- function(data, group) {
  UseMethod("group_prop", data)
}

#' @rdname group_prop
#' @method group_prop data.frame
#' @export
group_prop.data.frame <- function(data, group) {
  n <- nrow(data)
  data |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarize(size = dplyr::n() / n) |>
    as_named_vec(val = "size", name = group)
}

#' @rdname group_prop
#' @method group_prop data.table
#' @export
group_prop.data.table <- function(data, group) {
  n <- nrow(data)
  data[, .(size = .N / n), keyby = group] |>
    as_named_vec(val = "size", name = group)
}


#' Recover Block-Specific Success and Total Counts for Bootstrap Null
#' @name boot_null_counts
#'
#' @description
#' Recovers the number of successes and total observations within each group
#' for use in constructing group-specific Beta posteriors for the parametric
#' bootstrap joint test.
#'
#' @param data Data holding the appropriate outcomes
#' @param success_col Column holding the outcomes.
#' @param group Column to group by, or `NULL` for the whole dataset.
#'
#' @returns An aggregated data.frame or data.table with the appropriate counts.
#'
#' @keywords internal
boot_null_counts <- function(data, success_col, group = NULL) {
  UseMethod("boot_null_counts", data)
}

#' @rdname boot_null_counts
#' @method boot_null_counts data.frame
#' @export
boot_null_counts.data.frame <- function(data, success_col, group = NULL) {
  if (!is.null(group)) {
    data |>
      dplyr::group_by(!!rlang::sym(group)) |>
      dplyr::summarize(n = dplyr::n(), s = sum(!!rlang::sym(success_col)))
  } else {
    data |>
      dplyr::summarize(n = dplyr::n(), s = sum(!!rlang::sym(success_col)))
  }
}

#' @rdname boot_null_counts
#' @method boot_null_counts data.table
#' @export
boot_null_counts.data.table <- function(data, success_col, group = NULL) {
  if (!is.null(group)) {
    data[, .(n = .N, s = sum(get(success_col))), keyby = group]
  } else {
    data[, .(n = .N, s = sum(get(success_col)))]
  }
}
#----------------------------------------------------------------------------#
