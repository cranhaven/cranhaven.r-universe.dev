#' Augment SEM data with predictions, residuals, SEs/CIs, and ordinal extras
#'
#' @description
#' User-facing wrapper that augments a fitted \pkg{lavaan} model with:
#' predicted observed values (\emph{yhat}), residuals (\emph{obs - yhat}),
#' delta-method standard errors and confidence intervals for predictions,
#' and—when the model includes ordinal indicators—latent linear predictors
#' (\emph{y*}) and per-category probabilities. Works for continuous-only,
#' ordinal-only, and mixed models by internally routing to specialized
#' helpers for each measurement type.
#'
#' @details
#' Internally, \code{augment()} delegates work to the internal functions
#' \code{.augment_continuous()} and \code{.augment_ordinal()}, each optimized for
#' their respective indicator type. The wrapper automatically detects whether
#' the fitted model contains continuous, ordinal, or mixed indicators and merges
#' outputs from both branches as needed.
#'
#' The function reuses optional inputs:
#' \itemize{
#'   \item \code{data}: precomputed factor scores (and optional FS SEs) as returned
#'         by \code{\link{lavPredict_parallel}()} to avoid duplicate work.
#'   \item \code{info}: model metadata from \code{\link{model_info}()}.
#' }
#' Column naming is controlled by prefix arguments. For ordinal probabilities,
#' names follow the pattern \code{<prefix_pr><category><sep><item>}, e.g.
#' \code{".pr_3__A1"} for category 3 of item \code{A1} when \code{sep = "__"}.
#' The final column order can be arranged either "by type" (all observed, then
#' all \code{y*}, all \code{yhat}, CIs, SEs, residuals, then probabilities) or
#' "by item" (grouping each item's block together) via \code{col_layout}.
#'
#' @param fit A fitted \code{lavaan} model object.
#' @param data Optional factor-score output to reuse. Either a \code{data.frame}
#'   (single-group) or a \code{list} of \code{data.frame}s (per group) as returned
#'   by \code{\link{lavPredict_parallel}()}. If \code{NULL}, it is computed once
#'   and reused internally.
#' @param info Optional \code{\link{model_info}()} list; if \code{NULL}, it is computed.
#'
#' @param yhat Logical; include predicted observed values. Default \code{TRUE}.
#' @param resid Logical; include residuals (\code{obs - yhat}). Default \code{TRUE}.
#' @param ci Logical; include delta-method confidence intervals for \code{yhat}.
#'   Default \code{TRUE}.
#' @param level Confidence level for \code{ci}. Default \code{0.95}.
#' @param se_yhat Logical; include delta-method standard errors of \code{yhat}.
#'   Default \code{TRUE}.
#'
#' @param ystar Logical; for ordinal items, include latent linear predictors \eqn{y^*}.
#'   Default \code{TRUE}.
#' @param pr Logical; for ordinal items, include per-category probabilities.
#'   Default \code{TRUE}.
#'
#' @param se_fs Logical; request factor-score SEs from \code{lavPredict_parallel()}
#'   for continuous-only models. Ignored when the model contains any ordinal
#'   indicators. Default \code{TRUE}.
#' @param vcov_type Optional \code{\link[stats]{vcov}} type passed to the continuous
#'   branch when computing robust SEs/CIs for predictions (e.g., \code{"sandwich"}).
#'
#' @param col_layout Column layout for the augmented output; either
#'   \code{"by_type"} (observed, \code{y*}, \code{yhat}, CI lower/upper, \code{se_yhat},
#'   residuals, probabilities) or \code{"by_item"} (per-item blocks). Default
#'   \code{c("by_type","by_item")} with \code{match.arg()} semantics.
#'
#' @param prefix_ystar Character prefix for latent linear predictor columns
#'   (ordinal), e.g., \code{".ystar_"}. Default \code{".ystar_"}.
#' @param prefix_yhat Character prefix for predicted observed values, e.g.,
#'   \code{".yhat_"}. Default \code{".yhat_"}.
#' @param prefix_pr Character prefix for ordinal category-probability columns,
#'   e.g., \code{".pr_"}. Default \code{".pr_"}.
#' @param prefix_ci Length-2 character vector with lower/upper prefixes for
#'   prediction intervals, e.g., \code{c(".yhat_lwr_", ".yhat_upr_")}. Default \code{c(".yhat_lwr_", ".yhat_upr_")}.
#' @param prefix_resid Character prefix for residual columns, e.g., \code{".resid_"}.
#'   Default \code{".resid_"}.
#' @param prefix_se_fs Character prefix for factor-score SE columns (continuous-only),
#'   e.g., \code{".se_"}. Default \code{".se_"}.
#' @param prefix_se_yhat Character prefix for prediction SE columns, e.g.,
#'   \code{".se_yhat_"}. Default \code{".se_yhat_"}.
#' @param sep Separator used in probability column names between category and item,
#'   e.g., \code{"__"} giving names like \code{".pr_2__Item1"}. Default \code{"__"}.
#'
#' @return A tibble-like \code{data.frame}. Columns appear in the following order and with the following rules:
#' \itemize{
#'   \item \strong{(i) Anchors first}: \code{.rid}, \code{.gid}, \code{.group} always lead the table.
#'   \item \strong{(ii) Original lavPredict columns next}: observed indicators, factor scores, and (optionally) factor-score SEs returned by \code{\link{lavPredict_parallel}()} follow immediately after anchors.
#'   \item \strong{(iii) FS SEs presence}: factor-score SE columns are present \emph{only} when \code{se_fs = TRUE} \emph{and} the model is continuous-only (i.e., contains no ordinal indicators).
#'   \item \strong{(iv) Augmentations}: per measurement type, adds \code{y*} (for ordinal), \code{yhat}, CI lower/upper, \code{se_yhat}, residuals, and (for ordinal) per-category probabilities.
#'   \item \strong{(v) Probability naming}: ordinal probability columns follow the pattern \code{<prefix_pr><cat><sep><item>} (e.g., \code{".pr_3__A1"}).
#' }
#' The relative order of the augmentation blocks (observed/\code{y*}/\code{yhat}/CIs/SEs/residuals/probabilities) is controlled by \code{col_layout}.
#'
#' @examples
#' # Continuous example
#' HS.model <- 'visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9'
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939,
#'                    meanstructure = TRUE)
#' augment(fit)
#'
#' \donttest{
#' # --- Ordinal example (discretize by quantiles; 5 ordered categories) -------
#' ord_items <- paste0("x", 1:9)
#' HS_ord <- lavaan::HolzingerSwineford1939
#' for (v in ord_items) {
#'   q <- stats::quantile(HS_ord[[v]], probs = seq(0, 1, length.out = 6), na.rm = TRUE)
#'   q <- unique(q)  # guard against duplicate cut points
#'   HS_ord[[v]] <- as.ordered(cut(HS_ord[[v]], breaks = q, include.lowest = TRUE))
#' }
#'
#' fit_ord <- lavaan::cfa(
#'   HS.model,
#'   data             = HS_ord,
#'   ordered          = ord_items,
#'   estimator        = "WLSMV",
#'   parameterization = "delta",
#'   meanstructure    = TRUE
#' )
#' augment(fit_ord)
#'
#' # --- Mixed example (x1–x3 ordinal, others continuous) ----------------------
#' mix_ord <- c("x1","x2","x3")
#' HS_mix  <- lavaan::HolzingerSwineford1939
#' for (v in mix_ord) {
#'   q <- stats::quantile(HS_mix[[v]], probs = seq(0, 1, length.out = 6), na.rm = TRUE)
#'   q <- unique(q)
#'   HS_mix[[v]] <- as.ordered(cut(HS_mix[[v]], breaks = q, include.lowest = TRUE))
#' }
#'
#' fit_mix <- lavaan::cfa(
#'   HS.model,
#'   data             = HS_mix,
#'   ordered          = mix_ord,
#'   estimator        = "WLSMV",
#'   parameterization = "delta",
#'   meanstructure    = TRUE
#' )
#' augment(fit_mix)
#' }
#' @family lavDiag-augmenters
#' @export
augment <- function(fit,
                    data            = NULL,
                    info            = NULL,
                    # common toggles
                    yhat            = TRUE,
                    resid           = TRUE,
                    ci              = TRUE,
                    level           = 0.95,
                    se_yhat         = TRUE,
                    # ordinal-only toggles
                    ystar           = TRUE,
                    pr              = TRUE,
                    # FS SEs (cont-only)
                    se_fs           = TRUE,
                    vcov_type       = NULL,
                    # NEW: column layout selector
                    col_layout      = c("by_type", "by_item"),
                    # prefixes (kept aligned with internals)
                    prefix_ystar    = ".ystar_",
                    prefix_yhat     = ".yhat_",
                    prefix_pr       = ".pr_",
                    prefix_ci       = c(".yhat_lwr_", ".yhat_upr_"),
                    prefix_resid    = ".resid_",
                    prefix_se_fs    = ".se_",
                    prefix_se_yhat  = ".se_yhat_",
                    sep             = "__") {

  col_layout <- match.arg(col_layout)

  # -- Assertions on fit (light; internals do strict checks) -------------------
  .assert_lavaan_fit(fit)

  # -- One-time model info -----------------------------------------------------
  if (is.null(info)) info <- model_info(fit)
  ov_all  <- info$observed_variables
  ov_cont <- info$ov_continuous
  ov_ord  <- info$ov_ordinal
  lats    <- info$latent_variables

  # -- One-time lavPredict data (reused) ---------------------------------------
  if (is.null(data)) {
    request_fs_se <- isTRUE(se_fs) && length(ov_ord) == 0L
    data <- lavPredict_parallel(
      fit,
      return_type  = "list",
      se           = request_fs_se,
      prefix_se_fs = prefix_se_fs
    )
  }

  # --- Helper: column ordering ------------------------------------------------
  # In English: Reorder columns into a tidy layout.
  .reorder_augmented_columns <- function(df, layout, info,
                                         prefix_ystar, prefix_yhat, prefix_ci,
                                         prefix_resid, prefix_se_fs, prefix_se_yhat,
                                         prefix_pr, sep) {

    # 0) Core anchors ----------------------------------------------------------
    core <- c(".rid", ".gid", ".group")
    core <- core[core %in% names(df)]

    # 1) Factor scores + their SEs --------------------------------------------
    lats <- info$latent_variables
    fs_cols    <- lats[lats %in% names(df)]
    fs_se_cols <- paste0(prefix_se_fs, fs_cols)
    fs_se_cols <- fs_se_cols[fs_se_cols %in% names(df)]

    # 2) Items to consider (observed variable names in model order) ------------
    items_model <- info$observed_variables
    items <- items_model[items_model %in% names(df) |
                           paste0(prefix_yhat, items_model) %in% names(df) |
                           paste0(prefix_ystar, items_model) %in% names(df) |
                           paste0(prefix_resid, items_model) %in% names(df) |
                           paste0(prefix_se_yhat, items_model) %in% names(df)]

    # Helper to safely keep existing columns only
    keep_existing <- function(x) x[x %in% names(df)]

    # 3) Build lists of columns by item / by type ------------------------------
    # Per-item collector (observed first, then generated in a stable order)
    collect_for_item <- function(j) {
      pr_regex <- paste0("^", gsub("([.\\+*?\\^$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", prefix_pr),
                         ".*", gsub("([.\\+*?\\^$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", sep),
                         j, "$")
      c(
        keep_existing(j),
        keep_existing(paste0(prefix_ystar,    j)),
        keep_existing(paste0(prefix_yhat,     j)),
        keep_existing(paste0(prefix_ci[1L],   j)),
        keep_existing(paste0(prefix_ci[2L],   j)),
        keep_existing(paste0(prefix_se_yhat,  j)),
        keep_existing(paste0(prefix_resid,    j)),
        grep(pr_regex, names(df), value = TRUE)
      )
    }

    # By-type collector: for each type, all items in model order
    collect_by_type <- function(prefix, suffix_fun = identity) {
      cols <- unlist(lapply(items, function(j) paste0(prefix, suffix_fun(j))), use.names = FALSE)
      keep_existing(cols)
    }
    # 'pr' by-type: preserve category order as in names(df)
    collect_pr_by_type <- function() {
      # all pr_* columns, but order them grouped by item
      pr_all <- grep(paste0("^", gsub("([.\\+*?\\^$\\(\\)\\[\\]\\{\\}\\|\\\\])","\\\\\\1", prefix_pr)), names(df), value = TRUE)
      out <- character(0)
      for (j in items) {
        rx <- paste0(prefix_pr, ".*", sep, j, "$")
        out <- c(out, grep(rx, pr_all, value = TRUE))
      }
      out
    }

    # 4) Build the middle block according to layout ----------------------------
    middle <- switch(layout,
                     "by_item" = unlist(lapply(items, collect_for_item), use.names = FALSE),
                     "by_type" = c(
                       keep_existing(items),  # raw observed items first
                       collect_by_type(prefix_ystar),
                       collect_by_type(prefix_yhat),
                       collect_by_type(prefix_ci[1L]),
                       collect_by_type(prefix_ci[2L]),
                       collect_by_type(prefix_se_yhat),
                       collect_by_type(prefix_resid),
                       collect_pr_by_type()
                     )
    )

    # 5) Anything leftover (unknown extras) goes to the end --------------------
    front <- c(core, fs_cols, fs_se_cols)
    known <- unique(c(front, middle))
    tail  <- setdiff(names(df), known)

    new_order <- c(front, middle, tail)
    df[, new_order, drop = FALSE]
  }

  # -- Routing by measurement type --------------------------------------------
  has_cont <- length(ov_cont) > 0L
  has_ord  <- length(ov_ord)  > 0L

  if (!has_cont && !has_ord) {
    stop("The fitted model has neither continuous nor ordinal observed indicators.", call. = FALSE)
  }

  if (has_cont && !has_ord) {
    out <- .augment_continuous(
      fit            = fit,
      data           = data,
      info           = info,
      yhat           = yhat,
      ci             = ci,
      level          = level,
      resid          = resid,
      se_fs          = se_fs,
      se_yhat        = se_yhat,
      prefix_yhat    = prefix_yhat,
      prefix_ci      = prefix_ci,
      prefix_resid   = prefix_resid,
      prefix_se_fs   = prefix_se_fs,
      prefix_se_yhat = prefix_se_yhat,
      vcov_type      = vcov_type
    )
    # Reorder here before returning
    out <- .reorder_augmented_columns(out, col_layout, info,
                                      prefix_ystar, prefix_yhat, prefix_ci,
                                      prefix_resid, prefix_se_fs, prefix_se_yhat,
                                      prefix_pr, sep)
    return(out)
  }

  if (!has_cont && has_ord) {
    out <- .augment_ordinal(
      fit            = fit,
      data           = data,
      info           = info,
      ystar          = ystar,
      yhat           = yhat,
      pr             = pr,
      ci             = ci,
      level          = level,
      resid          = resid,
      se_yhat        = se_yhat,
      prefix_ystar   = prefix_ystar,
      prefix_yhat    = prefix_yhat,
      prefix_pr      = prefix_pr,
      prefix_ci      = prefix_ci,
      prefix_resid   = prefix_resid,
      prefix_se_yhat = prefix_se_yhat,
      sep            = sep
    )
    out <- .reorder_augmented_columns(out, col_layout, info,
                                      prefix_ystar, prefix_yhat, prefix_ci,
                                      prefix_resid, prefix_se_fs, prefix_se_yhat,
                                      prefix_pr, sep)
    return(out)
  }

  # Mixed (both continuous and ordinal)
  aug_cont <- .augment_continuous(
    fit            = fit,
    data           = data,
    info           = info,
    yhat           = yhat,
    ci             = ci,
    level          = level,
    resid          = resid,
    se_fs          = se_fs,
    se_yhat        = se_yhat,
    prefix_yhat    = prefix_yhat,
    prefix_ci      = prefix_ci,
    prefix_resid   = prefix_resid,
    prefix_se_fs   = prefix_se_fs,
    prefix_se_yhat = prefix_se_yhat,
    vcov_type      = vcov_type
  )
  aug_ord <- .augment_ordinal(
    fit            = fit,
    data           = data,
    info           = info,
    ystar          = ystar,
    yhat           = yhat,
    pr             = pr,
    ci             = ci,
    level          = level,
    resid          = resid,
    se_yhat        = se_yhat,
    prefix_ystar   = prefix_ystar,
    prefix_yhat    = prefix_yhat,
    prefix_pr      = prefix_pr,
    prefix_ci      = prefix_ci,
    prefix_resid   = prefix_resid,
    prefix_se_yhat = prefix_se_yhat,
    sep            = sep
  )

  # Split base vs augmentation parts (unchanged)
  split_base_aug <- function(df) {
    aug_prefixes <- c(prefix_ystar, prefix_yhat, prefix_pr, prefix_resid, prefix_se_yhat,
                      prefix_ci[1L], prefix_ci[2L])
    starts_with_any <- function(nm, pref) any(startsWith(nm, pref))
    is_aug <- vapply(names(df), starts_with_any, logical(1L), pref = aug_prefixes)
    list(base = df[, !is_aug, drop = FALSE],
         aug  = df[,  is_aug, drop = FALSE])
  }
  sbc <- split_base_aug(aug_cont)
  sbo <- split_base_aug(aug_ord)

  base <- sbc$base
  if (all(c(".rid", ".gid") %in% names(base)) &&
      all(c(".rid", ".gid") %in% names(sbo$base))) {
    key_base <- paste0(base$.rid, "_", base$.gid)
    key_ord  <- paste0(sbo$base$.rid, "_", sbo$base$.gid)
    idx <- match(key_base, key_ord)
    if (!all(!is.na(idx) & idx == seq_along(idx))) {
      sbo$aug <- sbo$aug[idx, , drop = FALSE]
    }
  }
  dup_cols <- intersect(names(sbc$aug), names(sbo$aug))
  if (length(dup_cols)) sbo$aug <- sbo$aug[, setdiff(names(sbo$aug), dup_cols), drop = FALSE]

  out <- cbind(base, sbc$aug, sbo$aug, stringsAsFactors = FALSE)
  class(out) <- c("tbl_df", "tbl", "data.frame")

  # Final tidy column order
  out <- .reorder_augmented_columns(out, col_layout, info,
                                    prefix_ystar, prefix_yhat, prefix_ci,
                                    prefix_resid, prefix_se_fs, prefix_se_yhat,
                                    prefix_pr, sep)
  out
}
