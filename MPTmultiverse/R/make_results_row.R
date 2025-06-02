
########### data structure

# results <- tibble(
#   model = character(),
#   dataset = character(),
#   pooling = character(),
#   package = character(),
#   method = character(),
#   est_group = list(tibble()),
#   est_indiv = list(tibble()),
#   test_between = list(tibble()),
#   #est_cov = est_cov,
#   gof = list(tibble()),
#   gof_group = list(tibble()),
#   gof_indiv = list(tibble())
# )

# Create
#
# Internal function, creates container for results
#
# @param model       Character.
# @param dataset     Character.
# @param pooling     Character.
# @param package     Character.
# @param method      Character.
# @param data        A \code{data.frame}.
# @param id          Character.

#' @importFrom magrittr %>%
# @keywords internal

make_results_row <- function(
  model,
  dataset,
  pooling,
  package,
  method,
  data,
  id,
  condition,
  core = NULL  # character vector specifying which are core parameters
) {

  # prepare data to have the correct columns of id/condition
  data$id <- data[[id]]
  data$condition <- data[[condition]]

  conditions <- unique(data$condition)
  parameters <- as.character(MPTinR::check.mpt(model)$parameters)

  # check list of core parameters
  if (!missing(core) && !is.null(core)){
    stopifnot(is.vector(core) && is.character(core))
    stopifnot(all(core %in% parameters))
  }

  est_ind <- tibble::as_tibble(
    expand.grid(
      parameter = parameters
      , id = data$id
      , stringsAsFactors = FALSE
    )
  )

  est_ind <- dplyr::left_join(est_ind, data[, c("id", "condition")], by = "id")
  est_ind$core <- est_ind$parameter %in% core
  est_ind <- est_ind[,c("id", "condition", "parameter", "core")]
  est_ind <- tibble::add_column(est_ind, est = NA_real_, se = NA_real_)

  for (i in seq_along(getOption("MPTmultiverse")$ci_size)) {
    est_ind <- tibble::add_column(est_ind, xx = NA_real_)
    colnames(est_ind)[ncol(est_ind)] <- paste0("ci_", getOption("MPTmultiverse")$ci_size[i])
  }
  est_ind <- tibble::add_column(est_ind, identifiable = NA)

  # create est_group empty df
  est_group <- tibble::as_tibble(
    expand.grid(
      parameter = parameters
      , condition = unique(data$condition)
      , stringsAsFactors = FALSE
    )
  )
  est_group$core <- est_group$parameter %in% core
  est_group <- est_group[, c("condition", "parameter", "core")]
  est_group$est = NA_real_
  est_group$se = NA_real_

  for (i in seq_along(getOption("MPTmultiverse")$ci_size)) {
    est_group <- tibble::add_column(est_group, xx = NA_real_)
    colnames(est_group)[ncol(est_group)] <- paste0("ci_", getOption("MPTmultiverse")$ci_size[i])
  }

  # test_between: group comparisons ---------------------------------------
  if (length(conditions) > 1) {

    pairs <- utils::combn(
      x = conditions
      , m = 2
      , simplify = FALSE
    )

    tmp_test_between <- vector("list", length(pairs))

    for (i in seq_along(pairs)) {

      tmp_test_between[[i]] <- tibble::as_tibble(
        expand.grid(
          parameter = parameters
          , condition1 = pairs[[i]][1]
          , condition2 = pairs[[i]][2]
          , stringsAsFactors = FALSE
        )) %>%
        dplyr::mutate(core = .data$parameter %in% core) %>%
        dplyr::select(.data$parameter, .data$core,
                      .data$condition1, .data$condition2) %>%
        dplyr::mutate(est_diff = NA_real_, se = NA_real_, p = NA_real_)

      tibble_ci <- tibble::as_tibble(
        matrix(NA_real_, nrow(tmp_test_between[[i]]),
               length(getOption("MPTmultiverse")$ci_size),
               dimnames = list(NULL, paste0("ci_", getOption("MPTmultiverse")$ci_size))))
      tmp_test_between[[i]] <- dplyr::bind_cols(tmp_test_between[[i]], tibble_ci)
    }
    test_between <- dplyr::bind_rows(tmp_test_between)
  } else {
    # Return a zero-row tibble if no between-Ss condition is analyzed ----
    test_between <- tibble::tibble(
      parameter = character(0)
      , core = logical(0)
      , condition1 = character(0)
      , condition2 = character(0)
      , est_diff = numeric(0)
      , se = numeric(0)
      , p = numeric(0)
    )
    CI <- getOption("MPTmultiverse")$ci_size
    for (i in seq_along(CI)) {
      test_between[[paste0("ci_", CI[i])]] <- numeric(0)
    }
  }

  ## est_covariate <- ##MISSING

  # test_within -----------------------------------------------------------

  if(length(parameters) > 1) {
    pairs <- utils::combn(
      x = c(parameters)
      , m = 2
      , simplify = FALSE
    )

    tmp <- vector("list", length(pairs))

    for (i in seq_along(pairs)) {

      tmp[[i]] <- tibble::as_tibble(
        expand.grid(
          condition = conditions
          , parameter1 = pairs[[i]][1]
          , parameter2 = pairs[[i]][2]
          , stringsAsFactors = FALSE
        )) %>%
        dplyr::mutate(core1 = .data$parameter1 %in% core, core2 = .data$parameter2 %in% core) %>%
        dplyr::select(.data$condition, .data$parameter1, .data$parameter2, .data$core1, .data$core2) %>%
        dplyr::mutate(est = NA_real_, se = NA_real_, statistic = NA_real_, df = NA_real_, p = NA_real_)
    }
    test_within <- dplyr::bind_rows(tmp)

    CI <- getOption("MPTmultiverse")$ci_size
    for (i in seq_along(CI)) {
      test_within[[paste0("ci_", CI[i])]] <- NA_real_
    }
  } else {
    test_within <- tibble::tibble(
      condition = character(0L)
      , parameter1 = character(0L)
      , parameter2 = character(0L)
      , core1 = logical(0L)
      , core2 = logical(0L)
      , est = numeric(0L)
      , se = numeric(0L)
      , statistic = numeric(0)
      , df = numeric(0)
      , p = numeric(0L)
    )
  }


  # fungibility -----------------------------------------------------------
  if (method == "trait"){
    param_pairs <- utils::combn(x = parameters, m = 2 , simplify = FALSE)

    tmp_est_rho <- tmp_fungibility <- vector("list", length(pairs))
    for (i in seq_along(param_pairs)) {

      tmp_tibble <- tibble::as_tibble(
        expand.grid(
          parameter1 = param_pairs[[i]][1],
          parameter2 = param_pairs[[i]][2],
          condition = conditions,
          stringsAsFactors = FALSE
        )) %>%
        dplyr::mutate(core1 = .data$parameter1 %in% core,
                      core2 = .data$parameter2 %in% core) %>%
        dplyr::select(.data$parameter1, .data$parameter2,
                      .data$core1, .data$core2,
                      .data$condition)

      tmp_fungibility[[i]] <- tmp_tibble %>%
        dplyr::mutate(correlation = NA_real_)

      tibble_ci <- tibble::as_tibble(
        matrix(NA_real_, nrow(tmp_tibble),
               length(getOption("MPTmultiverse")$ci_size),
               dimnames = list(NULL, paste0("ci_", getOption("MPTmultiverse")$ci_size))))

      tmp_est_rho[[i]] <- tmp_tibble %>%
        dplyr::mutate(est = NA_real_,
                      se= NA_real_,
                      p = NA_real_) %>%
        dplyr::bind_cols(tibble_ci)
    }
    fungibility <- dplyr::bind_rows(tmp_fungibility) %>%
      dplyr::arrange(match(.data$condition, conditions))
    est_rho <- dplyr::bind_rows(tmp_est_rho) %>%
      dplyr::arrange(match(.data$condition, conditions))
  } else {
    est_rho <- tibble::tibble(condition = character(), parameter1 = character(), parameter2 = character())
    fungibility <- tibble::tibble(condition = character(), parameter1 = character(), parameter2 = character())
  }

  # create gof empty df ---------------------------------------------------
  gof <- tibble::tibble(
    type = "",
    focus = "",
    stat_obs = NA_real_,
    stat_pred = NA_real_,
    stat_df = NA_real_,
    p = NA_real_
  )

  # Create gof_group and gof_indiv ----
  # Exploits value recycling of `data.frame`
  gof_group <- tibble::as_tibble(
    data.frame(
      condition = unique(data$condition)
      , gof
      , stringsAsFactors = FALSE
    )
  )

  gof_indiv <- tibble::as_tibble(
    data.frame(
      data[, c("id", "condition")]
      , gof
      , stringsAsFactors = FALSE
    )
  )

  test_homogeneity <- tibble::tibble(
    condition = unique(data$condition)
    , chisq = NA_real_
    , df = NA_real_
    , p = NA_real_
  )

  # some overall statistics of estimation, currently only holds needed time
  estimation <- tibble::tibble(
    condition = c("complete_data", unique(data$condition), "individual")
    , time_difference = as.difftime(NA_real_, units = "secs")
  )


  # save used options in a tidy format ----
  used_options <- tidy_options(mpt_options())



  ## data structure for results
  tibble::tibble(
    model = model,
    dataset = dataset,
    pooling = pooling,
    package = package,
    method = method,
    est_group = list(est_group),
    est_indiv = list(est_ind),
    est_rho = list(est_rho),
    test_between = list(test_between),
    test_within = list(test_within),
    gof = list(gof),
    gof_group = list(gof_group),
    gof_indiv = list(gof_indiv),
    fungibility = list(fungibility),
    test_homogeneity = list(test_homogeneity),
    convergence = list(tibble::tibble()),
    estimation = list(estimation),
    options = list(used_options)
  )
}
