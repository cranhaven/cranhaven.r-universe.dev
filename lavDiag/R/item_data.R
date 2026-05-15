#' Compute Empirical and Model-Based Item Curves with Fit Diagnostics
#'
#' @description
#' High-level wrapper that combines model-based (from `augment()`) and empirical
#' (from GAM fits) item-level predictions for continuous and ordinal indicators.
#' For each item, smooth GAM curves are estimated as functions of the latent
#' variables on which the item loads, optionally in a multi-group setup.
#'
#' The function returns both the augmented dataset containing model-based and
#' empirical predictions (`original_data`), a table of item-level fit metrics
#' (`metrics`), and optionally predicted empirical values for latent grids
#' (`new_data`).
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Verifies that the input `lavaan`/`blavaan` model converged and
#'         contains latent variables.
#'   \item Extracts factor loadings and thresholds per group.
#'   \item Runs parallelized GAM fitting (`mgcv::gam`) for each item and group,
#'         using either Gaussian, Beta, or ordinal (`ocat`) families.
#'   \item Produces empirical item curves and agreement metrics between
#'         model-based and empirical predictions (R², RMSE, MAE, and penalized variants).
#'   \item Optionally, predicts empirical curves for latent grids produced by `prepare()`.
#' }
#'
#' @param fit A fitted `lavaan` or `blavaan` model object. Must include latent variables.
#' @param data Optional data frame with observed indicators used in the model.
#'   If `NULL`, data are taken from the fitted model.
#' @param info Optional output of \code{\link{model_info}}. If `NULL`, it will be computed automatically.
#' @param level Numeric; confidence level for prediction intervals (default = 0.95).
#' @param fam_cont A `mgcv` family object for continuous indicators
#'   (default = `mgcv::betar(link = "logit")`).
#' @param fam_ord Accepts either the string `"ocat"` or any valid `mgcv` family object for ordinal indicators.
#'   In mixed models, the function automatically selects the appropriate branch for each indicator type.
#' @param gam_args_cont A named list of arguments passed to GAM fitting for continuous items.
#' @param gam_args_ord A named list of arguments passed to GAM fitting for ordinal items.
#' @param plan Parallelization backend; one of `"auto"`, `"multisession"`, `"multicore"`,
#'   `"sequential"`, `"cluster"`, or `"none"`.
#' @param workers Optional integer; number of parallel workers to use.
#' @param cluster Optional external cluster object (e.g., from `parallel::makeCluster()`).
#' @param progress Logical; whether to display a progress bar (default = `FALSE`).
#' @param verbose Logical; whether to print progress messages (default = `TRUE`).
#' @param store_fits Logical; whether to store fitted GAM models and use them to compute
#'   predictions for latent grids via `prepare()` (default = `TRUE`).
#'
#' @return
#' A list with three elements:
#' \describe{
#'   \item{`original_data`}{A `tibble` containing the augmented original dataset with both
#'     model-based (`m_est_yhat_*`) and empirical (`e_est_yhat_*`) predictions for each item.}
#'   \item{`metrics`}{A `tibble` summarizing item-level fit indices for each group and item:
#'     \itemize{
#'       \item `r2`, `rmse`, `mae`: agreement between model and empirical fits.
#'       \item `r2_pen`, `rmse_pen`, `mae_pen`: penalized variants accounting for model complexity.
#'       \item `c_m`, `c_e`, `k_eff`: effective model and empirical complexity measures.
#'     }}
#'   \item{`new_data`}{Optional `tibble` of latent-grid predictions including empirical
#'     estimates (`e_est_*` etc.); present only when `store_fits = TRUE`.}
#' }
#'
#' @section Parallelization:
#' Parallel execution is handled via `furrr::future_map()` and controlled by `plan`,
#' `workers`, and `cluster`. Default is `"auto"`, which attempts to use an optimal backend
#' based on the operating system.
#'
#' @section Agreement Metrics:
#' \itemize{
#'   \item \deqn{R^2 = \mathrm{cor}^2(m, e)}
#'   \item \deqn{\mathrm{RMSE} = \sqrt{\frac{1}{n}\sum_{i=1}^{n} (m_i - e_i)^2}}
#'   \item \deqn{\mathrm{MAE} = \frac{1}{n}\sum_{i=1}^{n} |m_i - e_i|}
#' }
#'
#' @seealso
#' \code{\link{augment}}, \code{\link{prepare}}, \code{\link{model_info}},
#' \code{\link[mgcv]{gam}}, \code{\link[furrr]{future_map}}
#'
#' @family lavDiag-augmenters
#'
#' @examples
#' \donttest{
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939,
#'                    meanstructure = TRUE)
#' item_data(fit)
#' }
#' @export
item_data <- function(fit,
                      data     = NULL,
                      info     = NULL,
                      level    = 0.95,
                      fam_cont = mgcv::betar(link = "logit"),
                      fam_ord  = "ocat",
                      gam_args_cont = list(method = "REML"),
                      gam_args_ord  = list(method = "REML",
                                           select = TRUE),
                      plan     = c("auto","multisession","multicore",
                                   "sequential","cluster","none"),
                      workers  = NULL,
                      cluster  = NULL,
                      progress = FALSE,
                      verbose  = TRUE,
                      store_fits = TRUE) {

  plan <- match.arg(plan)
  vmsg <- function(...) { if (isTRUE(verbose)) message(...) }

  required_helpers <- c(
    ".fit_gam_models", ".predict_gam_cols",
    ".rescale_01", ".sv01", ".clamp01", ".linkinv_fun"
  )
  is_ocat <- (is.character(fam_ord) && identical(fam_ord, "ocat")) || (isTRUE(inherits(fam_ord, "family")) && identical(fam_ord$family, "ocat"))
  if (is_ocat) required_helpers <- c(required_helpers, ".predict_ocat_ci_delta")

  missing_helpers <- required_helpers[!vapply(required_helpers, function(nm) exists(nm, mode = "function", inherits = TRUE), logical(1))]
  if (length(missing_helpers)) {
    stop("Missing helper functions: ", paste(missing_helpers, collapse = ", "),
         ". Please source/load them before calling item_fit_data().")
  }
  fit_fun    <- .fit_gam_models
  pred_fun   <- .predict_gam_cols
  rescale_fn <- .rescale_01
  sv_fn      <- .sv01
  clamp_fn   <- .clamp01
  if (is_ocat) pred_ocat <- .predict_ocat_ci_delta

  vmsg("Starting item_fit_data()...")
  .assert_lavaan_fit(fit, require_converged = TRUE, require_latent = TRUE, forbid_multilevel = TRUE)
  if (is.null(info)) info <- model_info(fit)

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for empirical curves.")
  }
  if (isTRUE(inherits(fam_cont, "family")) && identical(fam_cont$family, "Beta regression")) {
    vmgcv <- utils::packageVersion("mgcv")
    if (vmgcv < "1.8.31") {
      stop("mgcv >= 1.8-31 is required for mgcv::betar(). Installed: ", as.character(vmgcv))
    }
  }

  ov_all   <- info$observed_variables
  ov_cont  <- info$ov_continuous
  ov_ord   <- info$ov_ordinal
  lats     <- info$latent_variables
  n_groups <- info$n_groups %||% 1L
  gvar     <- info$group_var %||% ".group"

  vmsg("Computing model-based curves (augment)...")
  out <- augment(
    fit            = fit,
    data           = data,
    info           = info,
    yhat = TRUE, ci = TRUE, resid = TRUE,
    se_yhat        = FALSE, ystar = FALSE, pr = FALSE,
    se_fs          = TRUE,
    prefix_yhat    = "m_est_yhat_",
    prefix_ci      = c("m_lwr_yhat_", "m_upr_yhat_"),
    col_layout     = "by_type"
  )
  vmsg("...augment done.")

  vmsg("Extracting loadings/thresholds & preparing per-group maps...")
  PE_raw <- lavaan::parameterEstimates(fit, standardized = FALSE)

  PE_sel <- PE_raw[PE_raw$op == "=~" & PE_raw$rhs %in% ov_all, , drop = FALSE]
  keep_cols <- intersect(c("group","lhs","rhs","est"), names(PE_sel))
  PE_L <- PE_sel[, keep_cols, drop = FALSE]
  if (!"group" %in% names(PE_L)) PE_L$group <- rep_len(1L, nrow(PE_L))

  tol <- 1e-10
  if (nrow(PE_L) == 0L) {
    loads_by_item_overall <- stats::setNames(vector("list", length(ov_all)), ov_all)
  } else {
    loads_by_item_overall <- tapply(seq_len(nrow(PE_L)), PE_L$rhs, function(idx) {
      lhs <- PE_L$lhs[idx]; est <- PE_L$est[idx]
      unique(lhs[is.finite(est) & abs(est) > tol])
    })
    loads_by_item_overall <- loads_by_item_overall[ov_all]
  }

  PE_by_group <- split(PE_L, PE_L$group)
  loads_by_item_by_group <- lapply(PE_by_group, function(PEg) {
    if (!nrow(PEg)) return(stats::setNames(vector("list", length(ov_all)), ov_all))
    li <- tapply(seq_len(nrow(PEg)), PEg$rhs, function(idx) {
      lhs <- PEg$lhs[idx]; est <- PEg$est[idx]
      unique(lhs[is.finite(est) & abs(est) > tol])
    })
    li[ov_all[!ov_all %in% names(li)]] <- list(NULL)
    li[ov_all]
  })

  PE_thr <- PE_raw[PE_raw$op == "|" & PE_raw$lhs %in% ov_all, , drop = FALSE]
  if (!"group" %in% names(PE_thr)) PE_thr$group <- rep_len(1L, nrow(PE_thr))

  thr_counts_by_group <- lapply(split(PE_thr, PE_thr$group), function(PEg) {
    if (!nrow(PEg)) {
      return(list(
        all  = stats::setNames(integer(length(ov_all)), ov_all),
        free = stats::setNames(integer(length(ov_all)), ov_all)
      ))
    }
    tabs_all <- tapply(PEg$lhs, PEg$lhs, length)
    tabs_free <- tapply(seq_len(nrow(PEg)), PEg$lhs, function(idx) {
      f <- PEg$free[idx]; length(unique(f[f > 0]))
    })

    all_vec  <- stats::setNames(integer(length(ov_all)), ov_all)
    free_vec <- stats::setNames(integer(length(ov_all)), ov_all)
    all_vec[names(tabs_all)]   <- as.integer(tabs_all)
    free_vec[names(tabs_free)] <- as.integer(tabs_free)

    list(all = all_vec, free = free_vec)
  })

  set_equal <- function(a, b) {
    if (length(a) != length(b)) return(FALSE)
    all(sort(a) == sort(b))
  }
  vary_by_group <- any(vapply(ov_all, function(j) {
    overall <- loads_by_item_overall[[j]]
    if (is.null(overall)) return(TRUE)
    any(!vapply(loads_by_item_by_group, function(li) {
      set_equal(li[[j]] %||% character(0L), overall %||% character(0L))
    }, logical(1L)))
  }, logical(1L)))
  vmsg("...loadings/thresholds processed.")

  fam_for_item <- function(j) {
    if (j %in% ov_cont) return(fam_cont)
    return(fam_ord)
  }

  rng_by_item <- lapply(ov_all, function(j) {
    if (!is.null(data) && j %in% names(data)) {
      x <- data[[j]]
    } else if (j %in% names(out)) {
      x <- out[[j]]
    } else {
      x <- NULL
    }
    if (is.null(x)) return(c(0,1))
    if (is.factor(x)) c(1L, nlevels(x)) else range(as.numeric(x), na.rm = TRUE, finite = TRUE)
  })
  names(rng_by_item) <- ov_all

  ord_value_map <- lapply(ov_all, function(j) {
    x <- if (!is.null(data) && j %in% names(data)) data[[j]] else out[[j]]
    if (is.null(x)) return(NULL)
    if (is.factor(x) || is.ordered(x)) {
      levs <- levels(x)
      suppressWarnings({ lev_num <- as.numeric(levs) })
      if (all(is.finite(lev_num))) lev_num else seq_len(nlevels(x))
    } else if (j %in% ov_ord) {
      sort(unique(as.numeric(x)))
    } else {
      NULL
    }
  })
  names(ord_value_map) <- ov_all

  .emap_expect <- function(E_index, vals) {
    R <- length(vals)
    lo <- pmax(1, pmin(R - 1, floor(E_index)))
    hi <- lo + 1
    w  <- E_index - lo
    vals[lo] + w * (vals[hi] - vals[lo])
  }

  vmsg(sprintf("Fitting empirical GAM curves (%d group%s)...", n_groups, ifelse(n_groups > 1, "s", "")))
  reset_plan <- .set_future_plan(plan = plan, workers = workers, cluster = cluster)
  on.exit(reset_plan(), add = TRUE)

  rows_by_g <- if (".gid" %in% names(out)) lapply(seq_len(n_groups), function(g) which(out$.gid == g)) else list(seq_len(nrow(out)))
  names(rows_by_g) <- as.character(seq_len(n_groups))

  eta_cols <- intersect(lats, names(out))
  if (!length(eta_cols)) stop("Factor scores missing in data; cannot build empirical curves.")

  tasks <- expand.grid(g = seq_len(n_groups), j = ov_all, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  task_results <- furrr::future_map(
    seq_len(nrow(tasks)),
    function(ii) {
      g <- tasks$g[[ii]]
      j <- tasks$j[[ii]]
      rows_g <- rows_by_g[[as.character(g)]]
      if (!length(rows_g) || !j %in% names(out)) return(NULL)

      rel_lats <- if (vary_by_group) {
        li_g <- loads_by_item_by_group[[as.character(g)]] %||% loads_by_item_by_group[[g]]
        (if (!is.null(li_g)) li_g[[j]] else NULL) %||% loads_by_item_overall[[j]]
      } else {
        loads_by_item_overall[[j]]
      }
      rel_lats <- intersect(rel_lats %||% eta_cols, eta_cols)
      if (!length(rel_lats)) return(NULL)

      df_g <- out[rows_g, c(j, rel_lats), drop = FALSE]
      fam_j <- fam_for_item(j)
      rj    <- rng_by_item[[j]]

      key <- paste(rel_lats, collapse = "|")
      sm_terms <- paste(sprintf("s(%s)", rel_lats), collapse = " + ")
      fml <- stats::as.formula(paste("y_resp ~", sm_terms))

      ok    <- stats::complete.cases(df_g[, c(j, rel_lats), drop = FALSE])
      df_ok <- df_g[ok, , drop = FALSE]
      if (!nrow(df_ok)) {
        n <- nrow(df_g)
        return(list(g = g, j = j,
                    mu  = rep(NA_real_, n),
                    lwr = rep(NA_real_, n),
                    upr = rep(NA_real_, n),
                    ce  = NA_real_,
                    fit = NULL))
      }

      if (j %in% ov_cont) {
        use_beta <- isTRUE(inherits(fam_j, "family")) && identical(fam_j$family, "Beta regression")
        if (use_beta) {
          y_fit <- sv_fn(rescale_fn(df_ok[[j]]))
          X_fit <- df_ok[, rel_lats, drop = FALSE]
          if (nrow(X_fit) >= length(rel_lats) + 2L) {
            fit_gam <- fit_fun(
              data    = cbind(y_resp = y_fit, X_fit),
              formula = fml,
              family  = fam_j,
              args    = gam_args_cont
            )
            nd <- out[rows_g, rel_lats, drop = FALSE]
            pred_df <- pred_fun(dat = nd, fit = fit_gam,
                                prefixes = c(".tmp_est_", ".tmp_lwr_", ".tmp_upr_"),
                                suffix   = "", level = level)
            to_original_scale <- function(p01) { d <- rj[2] - rj[1]; p01 * d + rj[1] }
            mu  <- to_original_scale(clamp_fn(pred_df[[1L]]))
            lwr <- to_original_scale(clamp_fn(pred_df[[2L]]))
            upr <- to_original_scale(clamp_fn(pred_df[[3L]]))
            mu  <- pmin(rj[2], pmax(rj[1], mu))
            lwr <- pmin(rj[2], pmax(rj[1], lwr))
            upr <- pmin(rj[2], pmax(rj[1], upr))
            ce  <- sum(fit_gam$edf, na.rm = TRUE)
          } else {
            n <- nrow(df_g); mu <- lwr <- upr <- rep(NA_real_, n); ce <- NA_real_; fit_gam <- NULL
          }
        } else {
          y_fit <- df_ok[[j]]
          X_fit <- df_ok[, rel_lats, drop = FALSE]
          if (nrow(X_fit) >= length(rel_lats) + 2L) {
            fit_gam <- fit_fun(
              data    = cbind(y_resp = y_fit, X_fit),
              formula = fml,
              family  = fam_j,
              args    = gam_args_cont
            )
            nd <- out[rows_g, rel_lats, drop = FALSE]
            pred_df <- pred_fun(dat = nd, fit = fit_gam,
                                prefixes = c(".tmp_est_", ".tmp_lwr_", ".tmp_upr_"),
                                suffix   = "", level = level)
            mu  <- pred_df[[1L]]; lwr <- pred_df[[2L]]; upr <- pred_df[[3L]]
            ce  <- sum(fit_gam$edf, na.rm = TRUE)
          } else {
            n <- nrow(df_g); mu <- lwr <- upr <- rep(NA_real_, n); ce <- NA_real_; fit_gam <- NULL
          }
        }
      } else if (is_ocat) {
        y_ord <- df_ok[[j]]
        if (!is.ordered(y_ord)) y_ord <- ordered(y_ord)
        y_fit <- as.integer(y_ord)
        X_fit <- df_ok[, rel_lats, drop = FALSE]

        if (nrow(X_fit) >= length(rel_lats) + 2L) {
          fam_use <- fam_j
          if ((is.character(fam_j) && identical(fam_j, "ocat")) ||
              (isTRUE(inherits(fam_j, "family")) && identical(fam_j$family, "ocat"))) {
            Rj <- nlevels(y_ord)
            if (!is.finite(Rj) || Rj < 2L) stop(sprintf("ocat needs at least 2 categories for item '%s'", j))
            fam_use <- mgcv::ocat(link = "identity", R = Rj)
          }
          fit_gam <- fit_fun(
            data    = cbind(y_resp = y_fit, X_fit),
            formula = fml,
            family  = fam_use,
            args    = gam_args_ord
          )
          nd <- out[rows_g, rel_lats, drop = FALSE]
          oc <- pred_ocat(fit_gam, newdata = nd, level = level)
          pr_mat <- try(suppressWarnings(stats::predict(fit_gam, newdata = nd, type = "response")), silent = TRUE)
          vals <- ord_value_map[[j]]
          if (is.matrix(pr_mat) && !is.null(vals) && ncol(pr_mat) == length(vals)) {
            mu  <- as.numeric(pr_mat %*% vals)
            lwr <- as.numeric(.emap_expect(oc$E_lwr, vals))
            upr <- as.numeric(.emap_expect(oc$E_upr, vals))
          } else {
            if (is.null(vals)) {
              to_original_from_scores <- function(E) {
                R <- length(fit_gam$family$getTheta(trans = TRUE)) + 1L
                if (!is.finite(rj[1]) || !is.finite(rj[2]) || R <= 1L) return(E)
                rj[1] + (E - 1) * ((rj[2] - rj[1]) / (R - 1))
              }
              mu  <- to_original_from_scores(oc$E_est)
              lwr <- to_original_from_scores(oc$E_lwr)
              upr <- to_original_from_scores(oc$E_upr)
            } else {
              mu  <- .emap_expect(oc$E_est, vals)
              lwr <- .emap_expect(oc$E_lwr, vals)
              upr <- .emap_expect(oc$E_upr, vals)
            }
          }
          mu  <- pmin(rj[2], pmax(rj[1], mu))
          lwr <- pmin(rj[2], pmax(rj[1], lwr))
          upr <- pmin(rj[2], pmax(rj[1], upr))
          ce  <- sum(fit_gam$edf, na.rm = TRUE)
        } else {
          n <- nrow(df_g); mu <- lwr <- upr <- rep(NA_real_, n); ce <- NA_real_; fit_gam <- NULL
        }
      } else {
        y_fit <- sv_fn(rescale_fn(df_ok[[j]]))
        X_fit <- df_ok[, rel_lats, drop = FALSE]
        if (nrow(X_fit) >= length(rel_lats) + 2L) {
          fit_gam <- fit_fun(
            data    = cbind(y_resp = y_fit, X_fit),
            formula = fml,
            family  = fam_j,
            args    = gam_args_ord
          )
          nd <- out[rows_g, rel_lats, drop = FALSE]
          pred_df <- pred_fun(dat = nd, fit = fit_gam,
                              prefixes = c(".tmp_est_", ".tmp_lwr_", ".tmp_upr_"),
                              suffix   = "", level = level)
          to_original_scale <- function(p01) { d <- rj[2] - rj[1]; p01 * d + rj[1] }
          mu  <- to_original_scale(clamp_fn(pred_df[[1L]]))
          lwr <- to_original_scale(clamp_fn(pred_df[[2L]]))
          upr <- to_original_scale(clamp_fn(pred_df[[3L]]))
          mu  <- pmin(rj[2], pmax(rj[1], mu))
          lwr <- pmin(rj[2], pmax(rj[1], lwr))
          upr <- pmin(rj[2], pmax(rj[1], upr))
          ce  <- sum(fit_gam$edf, na.rm = TRUE)
        } else {
          n <- nrow(df_g); mu <- lwr <- upr <- rep(NA_real_, n); ce <- NA_real_; fit_gam <- NULL
        }
      }

      list(g = g, j = j, mu = as.numeric(mu), lwr = as.numeric(lwr), upr = as.numeric(upr), ce = ce, fit = if (store_fits) fit_gam else NULL)
    },
    .options  = furrr::furrr_options(
      seed     = TRUE,
      packages = c("mgcv","stats"),
      globals  = list(fit_fun = fit_fun, pred_fun = pred_fun,
                      rescale_fn = rescale_fn, sv_fn = sv_fn, clamp_fn = clamp_fn,
                      pred_ocat = if (is_ocat) pred_ocat else NULL,
                      is_ocat = is_ocat,
                      ov_cont = ov_cont, ov_ord = ov_ord,
                      loads_by_item_by_group = loads_by_item_by_group,
                      loads_by_item_overall = loads_by_item_overall,
                      vary_by_group = vary_by_group,
                      eta_cols = eta_cols,
                      rng_by_item = rng_by_item,
                      out = out,
                      fam_for_item = fam_for_item,
                      rows_by_g = rows_by_g,
                      level = level,
                      gam_args_cont = gam_args_cont,
                      gam_args_ord = gam_args_ord)
    ),
    .progress = progress
  )

  ce_map   <- vector("list", n_groups); names(ce_map) <- as.character(seq_len(n_groups))
  gam_fits <- vector("list", n_groups); names(gam_fits) <- as.character(seq_len(n_groups))

  for (res in task_results) {
    if (is.null(res)) next
    g <- as.character(res$g); j <- res$j
    rows_g <- rows_by_g[[g]]
    nm_est <- paste0("e_est_yhat_", j)
    nm_lwr <- paste0("e_lwr_yhat_", j)
    nm_upr <- paste0("e_upr_yhat_", j)
    out[rows_g, nm_est] <- res$mu
    out[rows_g, nm_lwr] <- res$lwr
    out[rows_g, nm_upr] <- res$upr
    if (is.null(ce_map[[g]])) ce_map[[g]] <- stats::setNames(numeric(length(ov_all)), ov_all)
    ce_map[[g]][[j]] <- res$ce
    if (store_fits) {
      if (is.null(gam_fits[[g]])) gam_fits[[g]] <- stats::setNames(vector("list", length(ov_all)), ov_all)
      gam_fits[[g]][[j]] <- res$fit
    }
  }

  vmsg("Empirical GAM curves done.")

  is_num <- vapply(out, is.numeric, logical(1L))
  keep_int <- names(out) %in% c(".rid", ".gid")
  to_double <- is_num & !keep_int
  for (nm in names(out)[to_double]) out[[nm]] <- as.double(out[[nm]])

  vmsg("Computing agreement metrics (r2, rmse, mae, penalized variants)...")
  metrics <- list()
  for (g in seq_len(n_groups)) {
    rows_g <- rows_by_g[[as.character(g)]]

    g_label <- if (n_groups > 1L) {
      if (gvar %in% names(out)) {
        gvals <- out[[gvar]][rows_g]
        idx   <- which(!is.na(gvals) & nzchar(as.character(gvals)))
        if (length(idx)) as.character(gvals[idx[1]]) else as.character(g)
      } else {
        if (!is.null(info$group_labels) && length(info$group_labels) >= g) {
          as.character(info$group_labels[g])
        } else {
          as.character(g)
        }
      }
    } else {
      if (!is.null(info$group_labels) && length(info$group_labels) >= 1L) {
        as.character(info$group_labels[1L])
      } else {
        "1"
      }
    }

    ce_vec  <- ce_map[[as.character(g)]]

    for (j in ov_all) {
      mcol <- paste0("m_est_yhat_", j)
      ecol <- paste0("e_est_yhat_", j)
      if (!all(c(mcol, ecol) %in% names(out))) next

      x <- out[rows_g, mcol, drop = TRUE]
      y <- out[rows_g, ecol, drop = TRUE]
      ok <- is.finite(x) & is.finite(y)
      n_eff <- sum(ok)
      if (n_eff < 2L || (stats::sd(x[ok]) == 0 && stats::sd(y[ok]) == 0)) {
        r2 <- rmse <- mae <- NA_real_
      } else {
        r  <- suppressWarnings(stats::cor(x[ok], y[ok]))
        r2 <- if (is.finite(r)) r^2 else NA_real_
        dif <- y[ok] - x[ok]
        rmse <- sqrt(mean(dif^2))
        mae  <- mean(abs(dif))
      }

      rel_lats <- if (vary_by_group) {
        li_g <- loads_by_item_by_group[[as.character(g)]] %||% loads_by_item_by_group[[g]]
        (if (!is.null(li_g)) li_g[[j]] else NULL) %||% loads_by_item_overall[[j]]
      } else {
        loads_by_item_overall[[j]]
      }
      rel_lats <- rel_lats %||% character(0L)
      n_rel <- length(rel_lats)

      if (j %in% ov_cont) {
        cm <- n_rel + 1L
        type_j <- "continuous"
      } else {
        thr_gc <- thr_counts_by_group[[as.character(g)]]
        n_thr_all  <- if (!is.null(thr_gc)) as.integer(thr_gc$all[[j]]  %||% 0L) else 0L
        n_thr_free <- if (!is.null(thr_gc)) as.integer(thr_gc$free[[j]] %||% 0L) else 0L
        n_thr <- if (is.finite(n_thr_all) && n_thr_all > 0L) n_thr_all else n_thr_free
        if (!is.finite(n_thr) || n_thr <= 0L) {
          xj <- out[[j]]
          if (is.factor(xj)) n_thr <- max(0L, nlevels(xj) - 1L) else n_thr <- 0L
        }
        cm <- n_rel + n_thr
        type_j <- "ordinal"
      }

      ce <- ce_vec[[j]]
      if (is.finite(ce)) {
        k_eff <- cm + ce
      } else {
        k_eff <- cm
      }

      if (is.finite(r2) && n_eff > (k_eff + 1)) {
        r2_pen <- 1 - (1 - r2) * (n_eff - 1) / max(1, n_eff - k_eff - 1)
      } else {
        r2_pen <- NA_real_
      }
      if (is.finite(rmse) && n_eff > k_eff) {
        mse <- rmse^2
        rmse_pen <- sqrt(mse * n_eff / max(1, n_eff - k_eff))
      } else {
        rmse_pen <- NA_real_
      }
      if (is.finite(mae) && n_eff > k_eff) {
        mae_pen <- mae * sqrt(n_eff / max(1, n_eff - k_eff))
      } else {
        mae_pen <- NA_real_
      }

      metrics[[length(metrics) + 1L]] <- list(
        .gid      = g,
        .group    = g_label,
        item      = j,
        type      = type_j,
        n_eff     = n_eff,
        c_m       = as.numeric(cm),
        c_e       = as.numeric(if (is.finite(ce)) ce else NA_real_),
        k_eff     = as.numeric(k_eff),
        r2        = r2,
        rmse      = rmse,
        mae       = mae,
        r2_pen    = r2_pen,
        rmse_pen  = rmse_pen,
        mae_pen   = mae_pen
      )
    }
  }
  metrics_tbl <- if (length(metrics)) tibble::as_tibble(do.call(rbind, lapply(metrics, tibble::as_tibble_row)))
  else tibble::tibble(
    .gid   = integer(),
    .group = character(),
    item   = character(),
    type   = character(),
    n_eff  = integer(),
    c_m    = double(),
    c_e    = double(),
    k_eff  = double(),
    r2     = double(),
    rmse   = double(),
    mae    = double(),
    r2_pen   = double(),
    rmse_pen = double(),
    mae_pen  = double()
  )
  vmsg("...metrics done.")

  new_data <- NULL
  if (store_fits) {
    vmsg("Building new_data via prepare() using original_data as input...")
    new_data <- tryCatch(
      prepare(
        fit   = fit,
        info  = info,
        data  = out,
        level = level,
        plan    = plan,
        workers = workers
      ),
      error = function(e) {
        warning(sprintf("prepare() failed inside item_fit_data(): %s", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(new_data)) vmsg("...prepare() done.") else vmsg("...prepare() failed (see warning).")
  }

  if (!is.null(new_data) && store_fits) {
    vmsg("Scoring empirical predictions for new_data latent grids...")
    if (!all(c(".gid", ".latent_var") %in% names(new_data))) {
      warning("new_data lacks .gid or .latent_var; skipping empirical predictions for new_data.")
    } else {
      for (g in seq_len(n_groups)) {
        rows_ng <- which(new_data$.gid == g)
        if (!length(rows_ng)) next
        nd_g <- new_data[rows_ng, , drop = FALSE]

        gf <- gam_fits[[as.character(g)]]
        if (is.null(gf)) next

        lat_in_row <- as.character(nd_g$.latent_var)

        for (j in ov_all) {
          fit_gj <- gf[[j]]
          if (is.null(fit_gj)) next

          rel_lats <- if (vary_by_group) {
            li_g <- loads_by_item_by_group[[as.character(g)]] %||% loads_by_item_by_group[[g]]
            (if (!is.null(li_g)) li_g[[j]] else NULL) %||% loads_by_item_overall[[j]]
          } else {
            loads_by_item_overall[[j]]
          }
          rel_lats <- rel_lats %||% character(0L)
          if (!length(rel_lats)) next

          for (k in unique(lat_in_row)) {
            if (!k %in% rel_lats) next
            sel_k <- which(lat_in_row == k)
            if (!length(sel_k)) next

            nd_pred <- nd_g[sel_k, rel_lats, drop = FALSE]

            fam_j <- fam_for_item(j)
            use_beta <- isTRUE(inherits(fam_j, "family")) && identical(fam_j$family, "Beta regression")

            if (is_ocat && (j %in% ov_ord)) {
              oc <- pred_ocat(fit_gj, newdata = nd_pred, level = level)
              pr_mat <- try(suppressWarnings(stats::predict(fit_gj, newdata = nd_pred, type = "response")), silent = TRUE)
              vals <- ord_value_map[[j]]
              if (is.matrix(pr_mat) && !is.null(vals) && ncol(pr_mat) == length(vals)) {
                mu  <- as.numeric(pr_mat %*% vals)
                lwr <- .emap_expect(oc$E_lwr, vals)
                upr <- .emap_expect(oc$E_upr, vals)
                pred_df <- data.frame(
                  stats::setNames(list(mu),  paste0("e_est_", j, "_", k)),
                  stats::setNames(list(lwr), paste0("e_lwr_", j, "_", k)),
                  stats::setNames(list(upr), paste0("e_upr_", j, "_", k))
                )
              } else {
                if (is.null(vals)) {
                  R <- length(fit_gj$family$getTheta(trans = TRUE)) + 1L
                  rj <- rng_by_item[[j]]
                  to_original_from_scores <- function(E) {
                    if (!is.finite(rj[1]) || !is.finite(rj[2]) || R <= 1L) return(E)
                    rj[1] + (E - 1) * ((rj[2] - rj[1]) / (R - 1))
                  }
                  pred_df <- data.frame(
                    stats::setNames(list(to_original_from_scores(oc$E_est)), paste0("e_est_", j, "_", k)),
                    stats::setNames(list(to_original_from_scores(oc$E_lwr)), paste0("e_lwr_", j, "_", k)),
                    stats::setNames(list(to_original_from_scores(oc$E_upr)), paste0("e_upr_", j, "_", k))
                  )
                } else {
                  pred_df <- data.frame(
                    stats::setNames(list(.emap_expect(oc$E_est, vals)), paste0("e_est_", j, "_", k)),
                    stats::setNames(list(.emap_expect(oc$E_lwr, vals)), paste0("e_lwr_", j, "_", k)),
                    stats::setNames(list(.emap_expect(oc$E_upr, vals)), paste0("e_upr_", j, "_", k))
                  )
                }
              }
            } else {
              pred_df <- pred_fun(
                dat      = nd_pred,
                fit      = fit_gj,
                prefixes = c("e_est_", "e_lwr_", "e_upr_"),
                suffix   = paste0(j, "_", k),
                level    = level
              )

              if ((j %in% ov_ord && !is_ocat) || use_beta) {
                rj <- rng_by_item[[j]]
                to_original_scale <- function(p01) { d <- rj[2] - rj[1]; p01 * d + rj[1] }
                cn <- names(pred_df)
                pred_df[[cn[1]]] <- to_original_scale(clamp_fn(pred_df[[cn[1]]]))
                pred_df[[cn[2]]] <- to_original_scale(clamp_fn(pred_df[[cn[2]]]))
                pred_df[[cn[3]]] <- to_original_scale(clamp_fn(pred_df[[cn[3]]]))
                pred_df[[cn[1]]] <- pmin(rj[2], pmax(rj[1], pred_df[[cn[1]]]))
                pred_df[[cn[2]]] <- pmin(rj[2], pmax(rj[1], pred_df[[cn[2]]]))
                pred_df[[cn[3]]] <- pmin(rj[2], pmax(rj[1], pred_df[[cn[3]]]))
              }
            }

            for (cn in names(pred_df)) {
              new_data[rows_ng[sel_k], cn] <- pred_df[[cn]]
            }
          }
        }
      }
    }
    vmsg("...empirical predictions for new_data done.")
  }


  vmsg("Assembling final output list...")
  return(list(
    original_data = tibble::as_tibble(out),
    metrics       = metrics_tbl,
    new_data      = new_data
  ))
}



