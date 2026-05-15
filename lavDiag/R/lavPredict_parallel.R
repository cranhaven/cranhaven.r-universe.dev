#' Fast & robust parallel wrapper for lavaan::lavPredict()
#'
#' Parallel, ordinal-aware, and mixed-safe implementation of `lavaan::lavPredict()`.
#'
#' ## Model-type behavior
#' - **Purely ordinal models:**
#'   Optionally reduces duplicated work via `dplyr::distinct()` on full rows.
#'   Prepends “all-categories” dummy rows per group to stabilize chunk predictions.
#'
#' - **Mixed models (ordinal + continuous):**
#'   No deduplication (predictions depend on continuous values).
#'   Additionally prepends “variance-insurance” dummy rows to ensure non-zero variance
#'   of all continuous variables within each group.
#'
#' ## Robust extreme-score handling
#' If `correct_extremes = TRUE`, only flagged rows are re-scored using a fallback method.
#' The flagging rule is controlled by `extreme_rule`:
#'
#' * `"auto"` (default): per-latent mixed metric — uses `"z_by_se"` where SE columns exist
#'   for that latent, otherwise robust `"mad"`. A row is flagged if **any** latent exceeds
#'   the threshold; the maximum across latents is used.
#' * `"z_by_se"`: flags rows where `abs(FS) / pmax(SE, eps) > extreme_k` for any latent.
#' * `"mad"`: robust Z: `abs(FS - median)/(1.4826 × MAD) > extreme_k`.
#' * `"abs"`: simple absolute threshold: `abs(FS) > extreme_k`.
#'
#' The re-scoring uses the first `fallback_method` supplied (default `"EMB"`),
#' and then automatically retries the other (e.g. `"EBM"`) if needed.
#'
#' If `flag_column = TRUE`, a logical column `.fs_corrected` marks corrected rows.
#' If `diagnostics = TRUE`, columns `.fs_rule` and `.fs_metric` are attached and an
#' attribute `fs_n_corrected` is added.  The internal `"mixed"` rule is always
#' reported as `"auto"` for user clarity.
#'
#' @param fit lavaan model object.
#' @param workers Integer; number of parallel workers
#'   (default = `max(1, parallel::detectCores() - 1)`).
#' @param plan One of `"auto"`, `"multisession"`, `"multicore"`, `"sequential"`.
#'   `"auto"` selects `"multisession"` cross-platform.
#' @param chunk_size Optional integer; number of rows per chunk (default computed adaptively).
#' @param return_type `"list"` or `"data"`; determines multi-group return format.
#' @param progress Logical; show `furrr` progress bar.
#' @param se Logical; if `TRUE` and model is continuous-only, attach standard-error
#'   columns for factor scores.
#' @param prefix_se_fs Character; prefix for SE columns (default = `".se_"`).
#' @param distinct One of `"never"`, `"auto"`, `"always"`; controls use of `distinct()`
#'   to shrink duplicated rows.
#' @param distinct_threshold Numeric; when `distinct = "auto"`, apply `distinct()`
#'   only if `nrow(data) >= distinct_threshold` (default = 5e4). Ignored for mixed models.
#' @param method Character; estimation method passed to `lavaan::lavPredict()`
#'   (e.g., `"ml"`, `"regression"`, `"EBM"`, `"EMB"`).
#' @param correct_extremes Logical; if `TRUE`, re-score only flagged rows via `fallback_method`.
#' @param extreme_rule One of `"auto"`, `"abs"`, `"z_by_se"`, `"mad"`.  See details above.
#' @param extreme_by One of `"auto"`, `"group"`, `"global"`.  If `"group"`, thresholds
#'   are computed within groups.
#' @param extreme_k Numeric; rule-dependent threshold (default = 3.5).
#' @param extreme_eps Small numeric to guard division by zero in z-scores (default = 1e-8).
#' @param fallback_method Character; fallback method(s) for corrected rows.
#'   Default tries `"EMB"` first, then `"EBM"` if that fails.
#' @param flag_column Logical; if `TRUE`, add column `.fs_corrected` marking corrected rows.
#' @param diagnostics Logical; if `TRUE`, attach diagnostic columns and attribute `fs_n_corrected`.
#' @param ... Additional arguments passed to `lavaan::lavPredict()`.
#'
#' @return
#' A tibble (single-group) or a list/tibble (multi-group, depending on `return_type`),
#' containing predicted factor scores and optionally SEs and diagnostics.
#'
#' @examples
#' \donttest{
#' # Convert selected indicators to ordinal
#' ord_items <- paste0("x", 1:9)
#' HS_ord <- lavaan::HolzingerSwineford1939
#' for (v in ord_items) {
#'   q <- stats::quantile(HS_ord[[v]], probs = seq(0, 1, length.out = 6), na.rm = TRUE)
#'   q <- unique(q)  # guard against duplicate cut points
#'   HS_ord[[v]] <- as.ordered(cut(HS_ord[[v]], breaks = q, include.lowest = TRUE))
#' }
#'
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#'
#' # Fit ordinal CFA model
#' fit_ord <- lavaan::cfa(
#'   HS.model,
#'   data             = HS_ord,
#'   ordered          = ord_items,
#'   estimator        = "WLSMV",
#'   parameterization = "delta",
#'   meanstructure    = TRUE
#' )
#'
#' # Parallel prediction with automatic extreme-score handling
#' lavPredict_parallel(fit_ord, correct_extremes = TRUE)
#' }
#' @export
lavPredict_parallel <- function(fit,
                                method             = "ml",
                                correct_extremes   = TRUE,
                                extreme_rule       = c("auto","abs","z_by_se","mad"),
                                extreme_by         = c("auto","group","global"),
                                extreme_k          = 4,
                                extreme_eps        = 1e-8,
                                fallback_method    = c("EMB","EBM"),
                                flag_column        = FALSE,
                                diagnostics        = FALSE,
                                workers            = NULL,
                                plan               = c("auto","multisession","multicore","sequential"),
                                chunk_size         = NULL,
                                return_type        = c("list","data"),
                                progress           = FALSE,
                                se                 = FALSE,
                                prefix_se_fs       = ".se_",
                                distinct           = c("never","auto","always"),
                                distinct_threshold = 5e4,
                                ...) {
  fallback_method <- match.arg(fallback_method)
  extreme_rule    <- match.arg(extreme_rule)
  extreme_by      <- match.arg(extreme_by)

  .assert_lavaan_fit(fit)
  plan        <- match.arg(plan)
  return_type <- match.arg(return_type)
  distinct    <- match.arg(distinct)

  info     <- model_info(fit)
  ov_all   <- info$observed_variables
  ov_ord   <- info$ov_ordinal
  ov_cont  <- info$ov_continuous
  is_mg    <- isTRUE(info$n_groups > 1L)
  gvar     <- info$group_var %||% ".group"
  has_ord  <- length(ov_ord)  > 0L
  has_cont <- length(ov_cont) > 0L

  if (is.null(workers)) workers <- max(1L, parallel::detectCores(logical = TRUE) - 1L)

  if (!is_mg) {
    dat_original <- tibble::as_tibble(lavaan::lavInspect(fit, "data"))
  } else {
    dat_original <- lavaan::lavInspect(fit, "data") |>
      purrr::map(tibble::as_tibble) |>
      dplyr::bind_rows(.id = gvar)
  }

  ov_ord <- ov_ord[ov_ord %in% names(dat_original)]

  if (!has_ord || workers <= 1L) {
    args_base <- list(
      object      = fit,
      newdata     = dat_original,
      append.data = TRUE,
      assemble    = TRUE,
      method      = method
    )
    if (is_mg) args_base$drop.list.single.group <- FALSE
    args_base <- c(args_base, rlang::dots_list(...))
    base <- do.call(lavaan::lavPredict, args_base)
    out  <- tibble::as_tibble(base)

    if (isTRUE(se)) {
      lv_names <- tryCatch(lavaan::lavNames(fit, type = "lv"), error = function(e) character())
      se_attr  <- tryCatch(attr(base, "se"), error = function(e) NULL)
      if (is.null(se_attr)) {
        args_se <- list(object = fit, newdata = dat_original, append.data = FALSE, assemble = TRUE, se = TRUE)
        if (is_mg) args_se$drop.list.single.group <- FALSE
        args_se <- c(args_se, rlang::dots_list(...))
        base_se <- tryCatch(do.call(lavaan::lavPredict, args_se), error = function(e) NULL)
        se_attr <- if (!is.null(base_se)) attr(base_se, "se") else NULL
      }
      if (!is.null(se_attr)) {
        if (is.list(se_attr)) {
          if (isTRUE(is_mg) && gvar %in% names(out)) {
            lev <- intersect(names(se_attr), as.character(out[[gvar]]))
            lev <- levels(factor(out[[gvar]], levels = lev))
            mats <- lapply(lev, function(gl) {
              mm <- se_attr[[gl]]
              if (is.null(mm)) return(NULL)
              as.matrix(mm)
            })
            ok <- !vapply(mats, is.null, logical(1))
            lev <- lev[ok]
            mats <- mats[ok]
            se_mat <- do.call(rbind, mats)
            grp_fac <- factor(out[[gvar]], levels = lev)
            cnt <- as.integer(table(grp_fac)); G <- length(cnt)
            if (nrow(se_mat) == G) {
              idx_rep <- rep(seq_len(G), times = cnt)
              se_mat  <- se_mat[idx_rep, , drop = FALSE]
            }
          } else {
            se_mat <- do.call(rbind, lapply(se_attr, function(x) as.matrix(x)))
          }
        } else {
          se_mat <- as.matrix(se_attr)
        }
        lv_pred_cols <- intersect(lv_names, colnames(out))
        if (length(lv_pred_cols)) {
          if (!is.null(se_mat)) {
            if (is.null(colnames(se_mat))) {
              if (ncol(se_mat) == length(lv_pred_cols)) colnames(se_mat) <- lv_pred_cols
              else colnames(se_mat) <- lv_pred_cols[seq_len(ncol(se_mat))]
            }
            n_out <- nrow(out); n_se <- nrow(se_mat)
            if (!identical(n_se, n_out)) {
              if (isTRUE(is_mg) && gvar %in% names(out)) {
                lev2 <- intersect(names(se_attr), as.character(out[[gvar]]))
                lev2 <- levels(factor(out[[gvar]], levels = lev2))
                grp_fac2 <- factor(out[[gvar]], levels = lev2)
                cnt2 <- as.integer(table(grp_fac2)); G2 <- length(cnt2)
                if (n_se == G2) {
                  idx_rep2 <- rep(seq_len(G2), times = cnt2)
                  se_mat   <- se_mat[idx_rep2, , drop = FALSE]
                  n_se     <- nrow(se_mat)
                }
              }
              # if (!identical(n_se, n_out)) {
              #   warning("SE matrix has unexpected number of rows (", n_se, "; expected ", n_out, "); skipping SE attachment.")
              #  se_mat <- NULL
              #}
            }
            if (!is.null(se_mat)) {
              keep <- intersect(colnames(se_mat), lv_pred_cols)
              if (length(keep)) {
                se_df <- as.data.frame(se_mat[, keep, drop = FALSE])
                names(se_df) <- paste0(prefix_se_fs, names(se_df))
                out <- dplyr::bind_cols(out, se_df)
              }
            }
          }
        }
      } else if (has_ord) {
        warning("SE not available from lavaan for ordinal models (ignored).")
      }
    }

    out <- (function(out_in) {
      if (!isTRUE(correct_extremes) || !has_ord || !identical(tolower(method), "ml")) return(out_in)
      lv_cols <- intersect(tryCatch(lavaan::lavNames(fit, type = "lv"), error = function(e) character()), names(out_in))
      if (!length(lv_cols)) return(out_in)

      res_m <- .metric_extreme(out_in, lv_cols, extreme_rule, extreme_k, extreme_eps, prefix_se_fs, by = extreme_by, gvar = gvar, is_mg = is_mg)
      flag  <- is.finite(res_m$metric) & (res_m$metric > extreme_k)
      if (diagnostics) {
        out_in$.fs_rule   <- if (identical(res_m$rule, "mixed")) "auto" else res_m$rule
        out_in$.fs_metric <- res_m$metric
      }
      if (!any(flag)) {
        if (diagnostics) attr(out_in, "fs_n_corrected") <- 0L
        return(out_in)
      }

      sub_df <- dat_original[flag, , drop = FALSE]
      sub_df$.ix <- seq_len(nrow(sub_df))

      dummy_ord  <- .create_allcat_dummy(dat_original, ov_ord, group_var = if (is_mg) gvar else NULL)
      dummy_cont <- .create_continuous_variance_dummy(dat_original, ov_cont, group_var = if (is_mg) gvar else NULL)
      dummy <- dplyr::bind_rows(dummy_ord, dummy_cont) |> dplyr::distinct()
      missing_cols <- setdiff(names(sub_df), names(dummy));
      if (length(missing_cols)) dummy <- .add_missing_cols_typed(dummy, sub_df, missing_cols)
      dummy <- dummy[, names(sub_df), drop = FALSE]; dummy$.ix <- NA_integer_
      nd <- dplyr::bind_rows(dummy, sub_df)

      try_methods <- c(fallback_method, setdiff(c("EMB","EBM"), fallback_method))
      pred_ok <- NULL; last_err <- NULL
      for (mth in try_methods) {
        args_f <- list(object = fit, newdata = nd, append.data = TRUE, assemble = TRUE, method = mth)
        if (is_mg) args_f$drop.list.single.group <- FALSE
        args_f <- c(args_f, rlang::dots_list(...))
        pred_ok <- tryCatch(
          suppressWarnings(do.call(lavaan::lavPredict, args_f)),
          error = function(e) { last_err <<- e; NULL }
        )
        if (!is.null(pred_ok)) break
      }
      if (is.null(pred_ok)) {
        wmsg <- if (!is.null(last_err)) conditionMessage(last_err) else "unknown error"
        warning("Extreme-correction failed (", wmsg, "); keeping ML scores.")
        if (diagnostics) attr(out_in, "fs_n_corrected") <- 0L
        return(out_in)
      }
      pred_ok <- tibble::as_tibble(pred_ok)
      pred_ok$.ix <- nd$.ix
      pred_ok <- pred_ok[!is.na(pred_ok$.ix), , drop = FALSE]
      pred_ok <- pred_ok[order(pred_ok$.ix), , drop = FALSE]

      keep_lv <- intersect(lv_cols, names(pred_ok))
      if (length(keep_lv)) {
        if (isTRUE(flag_column)) {
          out_in$.fs_corrected <- FALSE
          out_in$.fs_corrected[flag] <- TRUE
        }
        out_in[flag, keep_lv] <- pred_ok[, keep_lv, drop = FALSE]
      }
      if (diagnostics) attr(out_in, "fs_n_corrected") <- sum(flag)
      out_in
    })(out)

    if (!is_mg) return(out)
    if (return_type == "data") return(out)
    grp <- out[[gvar]]; out[[gvar]] <- NULL
    return(split(out, grp))
  }

  if (isTRUE(se)) {
    warning("`se=TRUE` is only supported for continuous-only models; ignoring for ordinal/mixed models.")
  }

  use_distinct <- switch(distinct,
                         "always" = TRUE,
                         "never"  = FALSE,
                         "auto"   = (nrow(dat_original) >= distinct_threshold))
  if (has_cont) use_distinct <- FALSE

  dat_driver <- if (use_distinct) dplyr::distinct(dat_original) else dat_original

  dat_driver$.ix <- seq_len(nrow(dat_driver))

  dummy_ord  <- .create_allcat_dummy(dat_original, ov_ord, group_var = if (is_mg) gvar else NULL)
  dummy_cont <- .create_continuous_variance_dummy(dat_original, ov_cont, group_var = if (is_mg) gvar else NULL)

  dummy <- dplyr::bind_rows(dummy_ord, dummy_cont) |> dplyr::distinct()

  missing_cols <- setdiff(names(dat_driver), names(dummy));
  if (length(missing_cols)) dummy <- .add_missing_cols_typed(dummy, dat_driver, missing_cols)
  dummy <- dummy[, names(dat_driver), drop = FALSE]
  dummy$.ix <- NA_integer_

  n_rows <- nrow(dat_driver)
  if (n_rows == 0L) return(dat_original)
  if (is.null(chunk_size)) {
    n_chunks   <- max(1L, workers)
    chunk_size <- ceiling(n_rows / n_chunks)
  }
  idx <- split(seq_len(n_rows), ceiling(seq_along(seq_len(n_rows)) / chunk_size))
  chunked <- lapply(idx, function(ix) dat_driver[ix, , drop = FALSE])

  if (nrow(dummy) > 0L) {
    chunked <- lapply(chunked, function(x) dplyr::bind_rows(dummy, x))
  }

  reset_plan <- .set_future_plan(plan = plan, workers = workers)
  on.exit(reset_plan(), add = TRUE)

  fopts <- list(append.data = TRUE, assemble = TRUE, method = method)
  if (is_mg) fopts$drop.list.single.group <- FALSE
  dots <- rlang::dots_list(...)

  pred_fun <- function(df_chunk) {
    pred <- do.call(lavaan::lavPredict, c(list(object = fit, newdata = df_chunk), fopts, dots))
    pred <- tibble::as_tibble(pred)
    pred$.ix <- df_chunk$.ix
    pred
  }

  out_list <- base::suppressWarnings(
    furrr::future_map(
      chunked,
      pred_fun,
      .progress = progress,
      .options  = furrr::furrr_options(
        seed     = TRUE,
        packages = c("lavaan","stats","dplyr","tibble"),
        globals  = TRUE
      )
    )
  )

  drop_dummy <- function(df) df[!is.na(df$.ix), , drop = FALSE]
  out_list <- lapply(out_list, drop_dummy)

  out_all <- tibble::as_tibble(do.call(vctrs::vec_rbind, out_list))

  if (use_distinct) {
    key_cols <- setdiff(names(dat_driver), ".ix")
    if (!length(key_cols)) stop("lavPredict_parallel: no key columns.", call. = FALSE)

    group_id_driver <- vctrs::vec_group_id(dat_driver[, key_cols, drop = FALSE])
    group_id_orig   <- vctrs::vec_group_id(dat_original[, key_cols, drop = FALSE])

    map_tbl <- tibble::tibble(.key = group_id_driver, .ix = dat_driver$.ix)
    map_tbl <- dplyr::distinct(map_tbl, rlang::.data$.key, rlang::.data$.ix)

    idx <- match(group_id_orig, map_tbl$.key)
    if (anyNA(idx)) stop("lavPredict_parallel: cannot map original rows to driver uniques.", call. = FALSE)
    ix_match <- map_tbl$.ix[idx]

    ord_ix <- match(ix_match, out_all$.ix)
    if (anyNA(ord_ix)) stop("lavPredict_parallel: prediction rows missing for mapped indices.", call. = FALSE)

    pred_cols <- setdiff(names(out_all), names(dat_driver))
    pred_mat  <- out_all[ord_ix, pred_cols, drop = FALSE]

    out_joined <- dplyr::bind_cols(dat_original, pred_mat)
  } else {
    ord_ix <- order(out_all$.ix)
    out_all <- out_all[ord_ix, , drop = FALSE]

    if (nrow(out_all) != nrow(dat_driver)) {
      stop("lavPredict_parallel: row count mismatch after dropping dummy rows.", call. = FALSE)
    }
    if (!identical(out_all$.ix, dat_driver$.ix)) {
      m <- match(dat_driver$.ix, out_all$.ix)
      if (anyNA(m)) stop("lavPredict_parallel: cannot align predictions to driver rows.", call. = FALSE)
      out_all <- out_all[m, , drop = FALSE]
    }

    pred_cols <- setdiff(names(out_all), names(dat_driver))
    pred_mat  <- out_all[, pred_cols, drop = FALSE]

    same_shape <- identical(nrow(dat_driver), nrow(dat_original)) &&
      identical(setdiff(names(dat_driver), ".ix"), names(dat_original))
    if (same_shape) {
      out_joined <- dplyr::bind_cols(dat_original, pred_mat)
    } else {
      base <- dat_driver
      out_all2 <- dplyr::bind_cols(base, pred_mat)
      out_joined <- dplyr::bind_cols(dat_original, tibble::tibble(.ix = seq_len(nrow(dat_original)))) |>
        dplyr::left_join(out_all2[, c(".ix", names(pred_mat)), drop = FALSE], by = ".ix") |>
        dplyr::select(-rlang::.data$.ix)
    }
  }

  out_joined <- (function(out_in){
    if (!isTRUE(correct_extremes) || !has_ord || !identical(tolower(method), "ml")) return(out_in)
    lv_cols <- intersect(tryCatch(lavaan::lavNames(fit, type = "lv"), error = function(e) character()), names(out_in))
    if (!length(lv_cols)) return(out_in)

    res_m <- .metric_extreme(out_in, lv_cols, extreme_rule, extreme_k, extreme_eps, prefix_se_fs, by = extreme_by, gvar = gvar, is_mg = is_mg)
    flag  <- is.finite(res_m$metric) & (res_m$metric > extreme_k)
    if (diagnostics) {
      out_in$.fs_rule   <- if (identical(res_m$rule, "mixed")) "auto" else res_m$rule
      out_in$.fs_metric <- res_m$metric
    }
    if (!any(flag)) {
      if (diagnostics) attr(out_in, "fs_n_corrected") <- 0L
      return(out_in)
    }

    sub_df <- dat_original[flag, , drop = FALSE]
    sub_df$.ix <- seq_len(nrow(sub_df))

    dummy_ord  <- .create_allcat_dummy(dat_original, ov_ord, group_var = if (is_mg) gvar else NULL)
    dummy_cont <- .create_continuous_variance_dummy(dat_original, ov_cont, group_var = if (is_mg) gvar else NULL)
    dummy <- dplyr::bind_rows(dummy_ord, dummy_cont) |> dplyr::distinct()
    missing_cols <- setdiff(names(sub_df), names(dummy));
    if (length(missing_cols)) dummy <- .add_missing_cols_typed(dummy, sub_df, missing_cols)
    dummy <- dummy[, names(sub_df), drop = FALSE]; dummy$.ix <- NA_integer_
    nd <- dplyr::bind_rows(dummy, sub_df)

    try_methods <- c(fallback_method, setdiff(c("EMB","EBM"), fallback_method))
    pred_ok <- NULL; last_err <- NULL
    for (mth in try_methods) {
      args_f <- list(object = fit, newdata = nd, append.data = TRUE, assemble = TRUE, method = mth)
      if (is_mg) args_f$drop.list.single.group <- FALSE
      args_f <- c(args_f, rlang::dots_list(...))
      pred_ok <- tryCatch(
        suppressWarnings(do.call(lavaan::lavPredict, args_f)),
        error = function(e) { last_err <<- e; NULL }
      )
      if (!is.null(pred_ok)) break
    }
    if (is.null(pred_ok)) {
      wmsg <- if (!is.null(last_err)) conditionMessage(last_err) else "unknown error"
      warning("Extreme-correction failed (", wmsg, "); keeping ML scores.")
      if (diagnostics) attr(out_in, "fs_n_corrected") <- 0L
      return(out_in)
    }
    pred_ok <- tibble::as_tibble(pred_ok)
    pred_ok$.ix <- nd$.ix
    pred_ok <- pred_ok[!is.na(pred_ok$.ix), , drop = FALSE]
    pred_ok <- pred_ok[order(pred_ok$.ix), , drop = FALSE]
    keep_lv <- intersect(lv_cols, names(pred_ok))
    if (length(keep_lv)) {
      if (isTRUE(flag_column)) {
        out_in$.fs_corrected <- FALSE
        out_in$.fs_corrected[flag] <- TRUE
      }
      out_in[flag, keep_lv] <- pred_ok[, keep_lv, drop = FALSE]
    }
    if (diagnostics) attr(out_in, "fs_n_corrected") <- sum(flag)
    out_in
  })(out_joined)

  if (!is_mg) return(out_joined)
  if (return_type == "data") return(out_joined)
  grp <- out_joined[[gvar]]; out_joined[[gvar]] <- NULL
  split(out_joined, grp)
}
