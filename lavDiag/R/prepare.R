#' Prepare smooth latent grids + model-based item curves (continuous, ordinal, or mixed)
#'
#' @description
#' Unified, robust wrapper that constructs synthetic latent-score grids and
#' model-based item curves for fitted `lavaan`/`blavaan` models. It tries both
#' internal branches and returns whichever applies:
#' - **Continuous branch** (for continuous indicators) and
#' - **Ordinal branch** (for ordinal indicators).
#'
#' If both succeed (mixed models), their outputs are joined on shared ID columns.
#' The function is designed to avoid manual handling of factor-score data or group
#' columns; that logic is delegated to sub-functions.
#'
#' @details
#' **Routing**
#' - Calls internal functions `.prepare_continuous()` and `.prepare_ordinal()` for continuous and ordinal items, respectively.
#' - Reuses `info` (from `model_info()`) and `data` (from `lavPredict_parallel()`) when provided, so each is computed at most once.
#'
#' **Merging**
#' - If only one branch applies, returns that result.
#' - If both apply, performs a `dplyr::full_join()` using shared keys among
#'   `c(".rid", ".gid", ".group", ".latent_var")` when available, otherwise on the
#'   intersection of column names.
#' - To prevent duplicated factor-score columns (e.g., `A.x`, `E.y`), the latent
#'   columns are kept from the **continuous** branch and dropped from the ordinal
#'   branch before the join.
#'
#' **Column semantics**
#' - Leading ID columns: `.rid`, `.gid`, `.group`, `.latent_var`.
#' - Latent columns follow directly after `.latent_var`.
#' - Continuous/ordinal model-based outputs use the common
#'   `m_est_*`, `m_lwr_*`, `m_upr_*` naming convention.
#'
#' @param fit A fitted `lavaan`/`blavaan` object.
#' @param data Optional factor-score table to reuse (either a single data frame
#'   or a per-group list) as typically returned by `lavPredict_parallel()`.
#'   If `NULL`, factor scores are obtained on demand by the sub-functions.
#' @param info Optional list from `model_info(fit)`. If `NULL`, it is computed once
#'   and forwarded to both branches.
#' @param plan Parallelization backend for the **ordinal** branch; one of
#'   `c("auto", "none", "multisession", "multicore", "sequential", "cluster")`.
#'   The continuous branch is fast and does not use futures.
#' @param workers Optional integer; number of workers used by future backends
#'   (ignored unless `plan` uses a parallel backend). Forwarded to the ordinal branch.
#' @param cluster Optional external cluster object created by
#'   `parallel::makeCluster()`; only used when `plan = "cluster"`. If supplied
#'   with a different `plan`, it is ignored.
#' @param ... Additional arguments passed unchanged to both sub-functions (e.g.,
#'   `level`, `vcov_type`, `length.out`, `other_latents`, `latent_var_as_factor`,
#'   `se`, `se_summary`).
#'
#' @return A tibble. For mixed models, it is the full join of the two branches
#'   using shared ID columns (see **Merging**). For single-type models, it is the
#'   single applicable branch.
#'
#' @section Parallelization:
#' - The function may set a safe fallback to `sequential` when executed inside a
#'   worker to prevent nested futures. External clusters are respected only with
#'   `plan = "cluster"`. `workers` is a hint for future backends and is ignored
#'   otherwise.
#'
#' @section Errors and warnings:
#' - If neither branch succeeds, the function aborts with a diagnostic message.
#' - When continuous and ordinal outputs have no shared columns, a row bind is
#'   returned with a `.source` column and a warning is issued.
#'
#' @seealso
#' Internal helpers .prepare_continuous() and .prepare_ordinal() (not exported);
#' see also `model_info()` and `lavPredict_parallel()`.
#'
#' @examples
#' # --- Continuous example --------------------------------------------------
#' HS.model <- 'visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9'
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939,
#'                    meanstructure = TRUE)
#' prepare(fit)
#'
#' \donttest{
#' # --- Ordinal example (discretize by quantiles; 5 ordered categories) -----
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
#' prepare(fit_ord)
#'
#' # --- Mixed example (x1–x3 ordinal, others continuous) --------------------
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
#' prepare(fit_mix)
#' }
#'
#' @family lavDiag-augmenters
#' @export

prepare <- function(fit,
                    data = NULL,
                    info = NULL,
                    plan = c("auto","none","multisession","multicore","sequential","cluster"),
                    workers = NULL,
                    cluster = NULL,
                    ...) {
  plan <- match.arg(plan)

  # Parallel args sanitization
  if (!identical(plan, "cluster")) {
    cluster <- NULL
  } else if (!is.null(cluster) && !inherits(cluster, "cluster")) {
    warning("'cluster' is not a valid parallel::makeCluster() object - ignoring.")
    cluster <- NULL
  }


  # Model info
  if (is.null(info)) info <- model_info(fit)
  eta_cols <- info$latent_variables

  # Branches
  p_cont <- tryCatch(
    .prepare_continuous(fit, data = data, info = info, ...),
    error = function(e) NULL
  )
  p_ord <- tryCatch(
    .prepare_ordinal(fit, data = data, info = info,
                     plan = plan, workers = workers, cluster = cluster, ...),
    error = function(e) NULL
  )

  if (is.null(p_cont) && is.null(p_ord)) {
    rlang::abort("Neither continuous nor ordinal branch succeeded - check model/functions.")
  }
  if (is.null(p_cont)) return(p_ord)
  if (is.null(p_ord))  return(p_cont)

  # Keep canonical factor-score columns (A, E, ...) only from the continuous branch
  if (length(eta_cols)) {
    p_ord <- dplyr::select(p_ord, -dplyr::any_of(eta_cols))
  }

  # Join
  id_candidates <- c(".rid", ".gid", ".group", ".latent_var")
  ids_cont <- intersect(id_candidates, colnames(p_cont))
  ids_ord  <- intersect(id_candidates, colnames(p_ord))
  by_ids   <- intersect(ids_cont, ids_ord)
  by <- if (length(by_ids)) by_ids else intersect(colnames(p_cont), colnames(p_ord))

  out <- if (!length(by)) {
    rlang::warn("Continuous and ordinal outputs have no shared columns - returning row bind.")
    dplyr::bind_rows(p_cont, p_ord, .id = ".source")
  } else {
    # With latent columns removed from p_ord, no A.x/E.y suffixes will be created
    dplyr::full_join(p_cont, p_ord, by = by)
  }

  id_front <- intersect(c(".rid", ".gid", ".group", ".latent_var"), colnames(out))
  if (length(id_front)) out <- dplyr::relocate(out, dplyr::all_of(id_front))
  out
}
