#' Helper functions for lavPredict_parallel()
#'
#' Internal utilities supporting parallelized factor score computation:
#' - Construction of dummy rows for ordinal and continuous variables.
#' - Safe column completion and type preservation.
#' - Metrics for detecting extreme factor scores.
#' These helpers are not exported and are designed for internal use only.
#'
#' @noRd
#' @keywords internal
.case_ids <- function(fit) {
  lavaan::lavInspect(fit, "case.idx")
}

# Ordinal dummy row construction ------------------------------------------
.create_allcat_dummy <- function(dat_original, ov_ord, group_var = NULL) {
  if (length(ov_ord) == 0L || nrow(dat_original) == 0L)
    return(dat_original[0, , drop = FALSE])

  ov_ord <- ov_ord[ov_ord %in% names(dat_original)]
  if (length(ov_ord) == 0L) return(dat_original[0, , drop = FALSE])

  build_rows_for_var <- function(df, var) {
    proto <- df[1, , drop = FALSE]
    x     <- df[[var]]
    cats  <- if (is.factor(x)) levels(x) else sort(unique(x))
    if (length(cats) == 0L) return(df[0, , drop = FALSE])

    rows <- lapply(cats, function(cat) {
      r <- proto
      r[[var]] <- .cast_like(cat, x)
      r
    })
    dplyr::bind_rows(rows)
  }

  if (is.null(group_var)) {
    parts <- lapply(ov_ord, function(v) build_rows_for_var(dat_original, v))
    dplyr::bind_rows(parts) |> dplyr::distinct()
  } else {
    dat_original |>
      dplyr::group_split(.data[[group_var]], .keep = TRUE) |>
      lapply(function(df_g) {
        parts_g <- lapply(ov_ord, function(v) build_rows_for_var(df_g, v))
        base_g  <- df_g |> dplyr::slice_head(n = 1)
        dplyr::bind_rows(base_g, parts_g) |> dplyr::distinct()
      }) |>
      dplyr::bind_rows() |> dplyr::distinct()
  }
}

# Continuous dummy row construction --------------------------------------
.create_continuous_variance_dummy <- function(df, cont_vars, group_var = NULL) {
  if (!length(cont_vars) || nrow(df) == 0L) return(df[0, , drop = FALSE])

  build_for_group <- function(dg) {
    if (nrow(dg) == 0L) return(dg[0, , drop = FALSE])
    proto1 <- dg[1, , drop = FALSE]
    proto2 <- dg[min(2L, nrow(dg)), , drop = FALSE]
    any_assigned <- FALSE

    for (v in cont_vars) {
      if (!v %in% names(dg)) next
      x <- dg[[v]]
      ux <- unique(x[is.finite(suppressWarnings(as.numeric(x))) & !is.na(x)])
      if (length(ux) < 2L) next
      proto1[[v]] <- .cast_like(ux[1], x)
      proto2[[v]] <- .cast_like(ux[2], x)
      any_assigned <- TRUE
    }

    if (!any_assigned) return(dg[0, , drop = FALSE])
    dplyr::bind_rows(proto1, proto2) |> dplyr::distinct()
  }

  if (is.null(group_var)) build_for_group(df)
  else df |> dplyr::group_split(.data[[group_var]], .keep = TRUE) |> lapply(build_for_group) |> dplyr::bind_rows()
}

# Typed column completion -------------------------------------------------
.add_missing_cols_typed <- function(dst, template, cols) {
  if (!length(cols)) return(dst)
  n <- nrow(dst)
  for (nm in cols) dst[[nm]] <- vctrs::vec_init(template[[nm]], n)
  dst
}

# Extreme-score diagnostics ----------------------------------------------
.resolve_rule <- function(df, lv_cols, rule, prefix_se) {
  if (!identical(rule, "auto")) return(rule)
  se_cols <- paste0(prefix_se, lv_cols)
  if (any(se_cols %in% names(df))) "mixed" else "mad"
}

.metric_core <- function(df, lv_cols, rule, eps, prefix_se) {
  if (!length(lv_cols) || nrow(df) == 0L) return(rep(NA_real_, nrow(df)))

  switch(rule,
         abs = {
           m <- do.call(pmax, c(lapply(lv_cols, function(nm) abs(as.numeric(df[[nm]]))), na.rm = TRUE))
         },
         z_by_se = {
           zlist <- lapply(lv_cols, function(nm) {
             se_nm <- paste0(prefix_se, nm)
             se_v  <- if (se_nm %in% names(df)) as.numeric(df[[se_nm]]) else NA_real_
             abs(as.numeric(df[[nm]])) / pmax(se_v, eps)
           })
           m <- do.call(pmax, c(zlist, na.rm = TRUE))
         },
         mixed = {
           zlist <- lapply(lv_cols, function(nm) {
             x <- as.numeric(df[[nm]])
             se_nm <- paste0(prefix_se, nm)
             if (se_nm %in% names(df)) {
               se_v <- as.numeric(df[[se_nm]])
               abs(x) / pmax(se_v, eps)
             } else {
               med <- stats::median(x[is.finite(x)], na.rm = TRUE)
               mad <- stats::mad(x[is.finite(x)], constant = 1.4826, na.rm = TRUE)
               if (!is.finite(mad) || mad <= 0) abs(x - med) else abs(x - med) / mad
             }
           })
           m <- do.call(pmax, c(zlist, na.rm = TRUE))
         },
         {
           zlist <- lapply(lv_cols, function(nm) {
             x <- as.numeric(df[[nm]])
             med <- stats::median(x[is.finite(x)], na.rm = TRUE)
             mad <- stats::mad(x[is.finite(x)], constant = 1.4826, na.rm = TRUE)
             if (!is.finite(mad) || mad <= 0) abs(x - med) else abs(x - med) / mad
           })
           m <- do.call(pmax, c(zlist, na.rm = TRUE))
         }
  )

  m[!is.finite(m)] <- NA_real_
  m
}

.metric_extreme <- function(df, lv_cols, rule, k, eps, prefix_se, by = c("auto", "group", "global"), gvar = NULL, is_mg = FALSE) {
  by <- match.arg(by)
  if (identical(by, "auto")) by <- if (isTRUE(is_mg) && !is.null(gvar) && gvar %in% names(df)) "group" else "global"
  if (!length(lv_cols) || nrow(df) == 0L) return(list(metric = rep(NA_real_, nrow(df)), rule = rule))

  rule_eff <- .resolve_rule(df, lv_cols, rule, prefix_se)

  if (identical(by, "group") && !is.null(gvar) && gvar %in% names(df)) {
    sp <- split(seq_len(nrow(df)), f = df[[gvar]], drop = TRUE)
    metric <- rep(NA_real_, nrow(df))
    for (ix in sp) metric[ix] <- .metric_core(df[ix, , drop = FALSE], lv_cols, rule_eff, eps, prefix_se)
  } else {
    metric <- .metric_core(df, lv_cols, rule_eff, eps, prefix_se)
  }

  list(metric = metric, rule = rule_eff)
}
