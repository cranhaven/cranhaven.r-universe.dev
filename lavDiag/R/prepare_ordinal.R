#' Prepare model-based item curves for ordinal indicators (fast version)
#'
#' Drop-in replacement for `prepare_ordinal()` focused on speed while preserving
#' output structure and column names (`m_est_*`, `m_lwr_*`, `m_upr_*`).
#'
#' Speed keys:
#' - Pre-index rows per (group, latent) only once.
#' - Pre-map parameter IDs (nu, lambda, thresholds) from `parTable()`.
#' - Align measurement pieces once per group.
#' - Scoped writes: compute in chunks and write to output in one pass.
#' - Optional parallelization over (group × item) via `future.apply` and your
#'   `.set_future_plan()` helper.
#'
#' @param fit lavaan object
#' @param info Optional `model_info(fit)` list
#' @param data Optional factor-score data (data.frame or list per group)
#' @param level CI level (default 0.95)
#' @param vcov_type Optional `lavaan::vcov()` type
#' @param length.out Grid length per latent (default 100)
#' @param other_latents Baseline for non-focal latents: "zero" or "mean"
#' @param latent_var_as_factor Encode `.latent_var` as factor
#' @param se Compute delta-method SEs for expectations (default TRUE)
#' @param se_summary Summary for FS SEs across individuals: "median" or "mean"
#' @param plan Future plan: one of c("auto","none","multisession","multicore","sequential","cluster")
#' @param workers Optional number of workers for parallel plans
#' @param cluster Optional cluster (for plan = "cluster")
#'
#' @return tibble like the original `prepare_ordinal()`
#' @noRd
#' @importFrom rlang .data
#' @keywords internal
.prepare_ordinal <- function(fit,
                             info                  = NULL,
                             data                  = NULL,
                             level                 = 0.95,
                             vcov_type             = NULL,
                             length.out            = 100L,
                             other_latents         = c("mean", "zero"),
                             latent_var_as_factor  = TRUE,
                             se                    = TRUE,
                             se_summary            = c("median","mean"),
                             plan                  = c("auto","none","multisession","multicore","sequential","cluster"),
                             workers               = NULL,
                             cluster               = NULL) {

  # -- Assertions --------------------------------------------------------------
  .assert_lavaan_fit(
    fit,
    require_converged     = TRUE,
    require_latent        = TRUE,
    require_meanstructure = NA,
    require_measurement   = "lambda",
    forbid_multilevel     = TRUE
  )

  other_latents <- match.arg(other_latents)
  se_summary    <- match.arg(se_summary)
  plan          <- match.arg(plan)
  stopifnot(is.numeric(length.out), length(length.out) == 1L, is.finite(length.out))
  length.out <- as.integer(length.out)
  if (length.out < 2L) rlang::abort("`length.out` must be >= 2.")

  prefix_se  <- ".se_"
  compute_se <- isTRUE(se)

  # -- Model info --------------------------------------------------------------
  if (is.null(info)) info <- model_info(fit)
  ov_all       <- info$observed_variables
  ov_ord       <- info$ov_ordinal
  latent_names <- info$latent_variables
  n_groups     <- info$n_groups %||% 1L
  gvar         <- info$group_var %||% ".group"
  g_labels     <- info$group_labels %||% as.character(seq_len(n_groups))
  param        <- info$parameterization %||% "delta"

  if (length(ov_ord) == 0L) rlang::abort("No ordinal indicators in the model.")
  if (length(latent_names) == 0L) rlang::abort("No latent variables detected.")

  # -- Factor scores for ranges (and optional SE summaries) --------------------
  fs_df <- if (is.null(data)) {
    lavPredict_parallel(fit, return_type = "data",
                        se = FALSE, prefix_se_fs = prefix_se)
  } else data

  # Allow per-group list input
  if (is.list(fs_df) && !is.data.frame(fs_df)) {
    fs_list <- fs_df
    fs_df <- dplyr::bind_rows(lapply(seq_along(fs_list), function(i) {
      df_g <- tibble::as_tibble(fs_list[[i]])
      gid  <- if (length(names(fs_list))) match(names(fs_list)[i], g_labels) else i
      if (is.na(gid)) gid <- i
      if (!".gid"   %in% names(df_g)) df_g$.gid   <- gid
      if (!".group" %in% names(df_g)) df_g$.group <- g_labels[gid]
      df_g
    }))
  }

  # Ensure group column is present/known
  if (n_groups > 1L && !gvar %in% names(fs_df)) {
    if (".group" %in% names(fs_df)) gvar <- ".group" else
      rlang::abort(paste0("Expected group column '", gvar, "' not found in factor-score data."))
  }

  # Normalize .group/.gid bookkeeping
  if (n_groups > 1L) {
    fs_df$.group <- as.character(fs_df[[gvar]])
    fs_df$.gid   <- as.integer(stats::setNames(seq_along(g_labels), g_labels)[fs_df$.group])
  } else {
    fs_df$.group <- g_labels[1L]
    fs_df$.gid   <- 1L
  }

  # -- Helpers -----------------------------------------------------------------
  rng_or_fallback <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(c(-3, 3))
    r <- range(x)
    if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) c(-3, 3) else r
  }

  # Group means of latents if requested
  means_by_g <- NULL
  if (identical(other_latents, "mean")) {
    means_by_g <- fs_df |>
      dplyr::group_by(.data$.gid) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(latent_names), ~mean(.x[is.finite(.x)]), .names = "{.col}"),
        .groups = "drop"
      )
  }

  # Optional group-wise summaries of FS SEs
  present_se <- intersect(paste0(prefix_se, latent_names), names(fs_df))
  se_summ <- NULL
  if (compute_se && length(present_se) > 0L) {
    fun <- if (se_summary == "median") stats::median else mean
    se_summ <- fs_df |>
      dplyr::group_by(.data$.gid) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(present_se), ~fun(.x[is.finite(.x)]), .names = "{.col}"),
        .groups = "drop"
      )
  }

  # -- Build the synthetic grid ------------------------------------------------
  grid_list <- vector("list", n_groups * length(latent_names))
  idx <- 0L
  for (g in seq_len(n_groups)) {
    base_vec <- stats::setNames(numeric(length(latent_names)), latent_names)
    if (!is.null(means_by_g)) {
      mu_row <- means_by_g[means_by_g$.gid == g, , drop = FALSE]
      if (nrow(mu_row) == 1L) for (ln in latent_names) base_vec[[ln]] <- as.numeric(mu_row[[ln]])
    }
    fs_g <- fs_df[fs_df$.gid == g, latent_names, drop = FALSE]
    for (k in seq_along(latent_names)) {
      kname <- latent_names[k]
      r_gk  <- rng_or_fallback(fs_g[[kname]])
      seq_k <- seq(from = r_gk[1], to = r_gk[2], length.out = length.out)
      mat   <- matrix(base_vec, nrow = length.out, ncol = length(base_vec), byrow = TRUE)
      colnames(mat) <- names(base_vec)
      mat[, kname]  <- seq_k

      tb <- tibble::tibble(
        .gid        = g,
        .group      = g_labels[g],
        .latent_var = if (isTRUE(latent_var_as_factor)) factor(kname, levels = latent_names) else kname,
        !!!as.data.frame(mat, stringsAsFactors = FALSE)
      )

      # attach per-group FS SE summaries as constants
      if (!is.null(se_summ)) {
        se_row <- se_summ[se_summ$.gid == g, , drop = FALSE]
        if (nrow(se_row) == 1L) {
          for (ln in latent_names) {
            nm <- paste0(prefix_se, ln)
            if (nm %in% names(se_row)) tb[[nm]] <- rep(as.numeric(se_row[[nm]]), length.out)
          }
        }
      }

      idx <- idx + 1L
      grid_list[[idx]] <- tb
    }
  }

  out <- dplyr::bind_rows(grid_list)
  out$.rid <- seq_len(nrow(out))

  # -- Column ordering ---------------------------------------------------------
  out <- out |>
    dplyr::relocate(".rid", .before = 1) |>
    dplyr::relocate(".gid", .after = ".rid") |>
    dplyr::relocate(".group", .after = ".gid") |>
    dplyr::relocate(".latent_var", .after = ".group") |>
    dplyr::relocate(dplyr::all_of(latent_names), .after = ".latent_var")

  if (!is.null(se_summ)) {
    se_cols <- paste0(prefix_se, latent_names)
    se_cols <- se_cols[se_cols %in% names(out)]
    if (length(se_cols)) {
      out <- dplyr::relocate(out, dplyr::all_of(se_cols),
                             .after = dplyr::all_of(latent_names[length(latent_names)]))
    }
  }

  # -- Row indices per (g, k) block -------------------------------------------
  if (is.factor(out$.latent_var)) {
    lv_levels <- levels(out$.latent_var)
    lv_int    <- as.integer(out$.latent_var)
    rows_idx <- lapply(seq_len(n_groups), function(g) {
      rg <- out$.gid == g
      stats::setNames(lapply(latent_names, function(kname) {
        k_int <- match(kname, lv_levels)
        which(rg & lv_int == k_int)
      }), latent_names)
    })
  } else {
    rows_idx <- lapply(seq_len(n_groups), function(g) {
      rg <- out$.gid == g
      stats::setNames(lapply(latent_names, function(kname) which(rg & out$.latent_var == kname)), latent_names)
    })
  }

  # -- Aligned measurement pieces ---------------------------------------------
  est   <- lavaan::lavInspect(fit, "est")
  est_l <- if (is.list(est) && "lambda" %in% names(est)) list(est) else est
  th0   <- tryCatch(lavaan::lavInspect(fit, "th"), error = function(e) NULL)
  th_l  <- if (is.null(th0)) replicate(n_groups, stats::setNames(numeric(0), character(0)), simplify = FALSE)
  else if (is.list(th0)) th0 else list(th0)
  if (length(th_l) == 1L && n_groups > 1L) th_l <- rep(th_l, n_groups)

  # -- vcov & parTable for delta-method ---------------------------------------
  if (compute_se) {
    V <- tryCatch({ if (is.null(vcov_type)) lavaan::vcov(fit) else lavaan::vcov(fit, type = vcov_type) }, error = function(e) NULL)
    if (is.null(V)) rlang::abort("vcov(fit) is not available; cannot compute delta-method CIs for ordinals.")
    pt <- lavaan::parTable(fit)
    npar_V   <- ncol(V)
    max_free <- suppressWarnings(max(pt$free[pt$free > 0], 0L))
    if (is.finite(max_free) && max_free > npar_V) {
      rlang::warn("Some free parameter indices exceed vcov dimension; those IDs will be ignored in SE computation.")
    }
  } else {
    V <- NULL; pt <- NULL
  }

  zcrit <- if (compute_se) stats::qnorm(1 - (1 - level)/2) else NA_real_

  # -- Align loadings/nu, residual scales if needed ---------------------------
  align_by_g <- vector("list", n_groups)
  sig_eps_by_g <- vector("list", n_groups)
  for (g in seq_len(n_groups)) {
    est_g <- est_l[[g]]
    lam_g <- est_g$lambda
    nu_g  <- est_g$nu %||% numeric(0)

    if (!is.null(colnames(lam_g))) lam_g <- lam_g[, latent_names, drop = FALSE]
    if (!is.null(rownames(lam_g))) lam_g <- lam_g[ov_ord, , drop = FALSE] else lam_g <- lam_g[match(ov_ord, ov_all), , drop = FALSE]
    if (is.null(nu_g) || length(nu_g) == 0L) { nu_g <- rep(0, length(ov_ord)); names(nu_g) <- ov_ord }
    else if (!is.null(names(nu_g))) nu_g <- nu_g[ov_ord] else nu_g <- nu_g[match(ov_ord, ov_all)]

    align_by_g[[g]] <- list(lambda = lam_g, nu = nu_g)

    if (identical(param, "theta")) {
      theta_g <- est_g$theta
      if (!is.null(theta_g)) {
        if (!is.null(rownames(theta_g))) sig_eps <- sqrt(diag(theta_g)[ov_ord]) else sig_eps <- sqrt(diag(theta_g)[match(ov_ord, ov_all)])
        sig_eps[!is.finite(sig_eps) | sig_eps <= 0] <- 1
      } else sig_eps <- rep(1, length(ov_ord))
      sig_eps_by_g[[g]] <- sig_eps
    } else {
      sig_eps_by_g[[g]] <- rep(1, length(ov_ord))
    }
  }

  base_eta_by_g <- lapply(seq_len(n_groups), function(g) {
    base <- stats::setNames(rep(0, length(latent_names)), latent_names)
    if (identical(other_latents, "mean") && !is.null(means_by_g)) {
      mu_row <- means_by_g[means_by_g$.gid == g, , drop = FALSE]
      if (nrow(mu_row) == 1L) for (ln in latent_names) base[[ln]] <- as.numeric(mu_row[[ln]])
    }
    base
  })

  # -- Map free-parameter IDs for delta-method --------------------------------
  id_map <- NULL
  if (compute_se) {
    id_map <- vector("list", n_groups)
    for (g in seq_len(n_groups)) {
      id_nu  <- stats::setNames(integer(length(ov_ord)), ov_ord)
      id_lam <- array(0L, dim = c(length(ov_ord), length(latent_names)), dimnames = list(ov_ord, latent_names))
      id_tau <- lapply(ov_ord, function(j) integer(0))

      for (jj in seq_along(ov_ord)) {
        j <- ov_ord[jj]
        ii <- which(pt$lhs == j & pt$op == "~1" & pt$group == g & pt$free > 0)
        id_val <- if (length(ii)) pt$free[ii][1] else 0L
        if (id_val > 0L && id_val <= ncol(V)) id_nu[j] <- id_val

        for (k in seq_along(latent_names)) {
          kname <- latent_names[k]
          ii <- which(pt$lhs == kname & pt$op == "=~" & pt$rhs == j & pt$group == g & pt$free > 0)
          id_val <- if (length(ii)) pt$free[ii][1] else 0L
          id_lam[j, kname] <- if (id_val > 0L && id_val <= ncol(V)) id_val else 0L
        }

        rows_tau <- which(pt$lhs == j & pt$op == "|" & pt$group == g & pt$free > 0)
        ids_tau  <- if (length(rows_tau)) pt$free[rows_tau] else integer(0)
        if (length(ids_tau)) ids_tau <- ids_tau[ids_tau > 0L & ids_tau <= ncol(V)]
        id_tau[[j]] <- ids_tau
      }

      id_map[[g]] <- list(id_nu = id_nu, id_lam = id_lam, id_tau = id_tau)
    }
  }

  # -- Threshold helpers & cached V[ids,ids] ----------------------------------
  get_tau_for <- function(th_vec, j) {
    nm <- names(th_vec); if (is.null(nm)) return(numeric(0))
    sel <- startsWith(nm, paste0(j, "|"))
    tau <- th_vec[sel]
    if (!length(tau)) return(numeric(0))
    idx <- suppressWarnings(as.integer(gsub("^.*\\|t(\\d+)$", "\\1", names(tau))))
    idx[is.na(idx)] <- seq_along(tau)
    as.numeric(tau[order(idx)])
  }
  .V_cache <- new.env(parent = emptyenv())
  .get_Vsub <- function(V, ids) {
    key <- paste0(ids, collapse = ",")
    v   <- .V_cache[[key]]
    if (!is.null(v)) return(v)
    v <- V[ids, ids, drop = FALSE]
    v <- (v + t(v)) * 0.5
    .V_cache[[key]] <- v
    v
  }

  # -- Parallel plan -----------------------------------------------------------
  reset_plan <- NULL
  if (plan != "none") {
    reset_plan <- .set_future_plan(plan = plan, workers = workers, cluster = cluster)
    on.exit({ if (!is.null(reset_plan)) reset_plan() }, add = TRUE)
  }

  # -- Precompute non-zero loading sets per (g,k) ------------------------------
  nz_j_by_gk <- vector("list", n_groups)
  for (g in seq_len(n_groups)) {
    lam_g <- align_by_g[[g]]$lambda
    nz_j_by_k <- vector("list", length(latent_names))
    for (k in seq_along(latent_names)) {
      nz_j_by_k[[k]] <- which(is.finite(lam_g[, k]) & lam_g[, k] != 0)
    }
    nz_j_by_gk[[g]] <- nz_j_by_k
  }

  # -- Tasks & chunking --------------------------------------------------------
  tasks_gk <- expand.grid(g = seq_len(n_groups), k = seq_along(latent_names), KEEP.OUT.ATTRS = FALSE)
  idx_all  <- seq_len(nrow(tasks_gk))
  chunk_sz <- 32L
  splits   <- split(idx_all, (seq_along(idx_all) - 1L) %/% chunk_sz)

  compute_one_gk <- function(g, k) {
    k_name <- latent_names[k]
    rows_k <- rows_idx[[g]][[k_name]]
    if (!length(rows_k)) return(NULL)

    eta_k    <- out[[k_name]][rows_k]
    base_eta <- base_eta_by_g[[g]]

    lam_g <- align_by_g[[g]]$lambda
    nu_g  <- align_by_g[[g]]$nu

    jj_vec <- nz_j_by_gk[[g]][[k]]
    if (!length(jj_vec)) return(NULL)

    out_chunks <- vector("list", length(jj_vec))

    for (idx_j in seq_along(jj_vec)) {
      jj <- jj_vec[idx_j]
      j  <- ov_ord[jj]

      tau_j <- get_tau_for(th_l[[g]], j)
      if (!length(tau_j)) { out_chunks[[idx_j]] <- NULL; next }
      m       <- length(tau_j) + 1L
      scores  <- seq_len(m)
      tau_pad <- c(-Inf, tau_j, Inf)

      sig_eps <- sig_eps_by_g[[g]][jj]

      lam_row <- lam_g[jj, , drop = TRUE]
      nz_idx  <- which(is.finite(lam_row) & lam_row != 0)

      ystar0 <- (nu_g[jj] %||% 0) + sum(lam_row[nz_idx] * base_eta[nz_idx])
      ystar  <- ystar0 + lam_row[k] * (eta_k - base_eta[k_name])

      if (identical(param, "theta")) {
        zmat  <- outer(ystar, tau_pad, function(y, t) (t - y) / sig_eps)
        inv_s <- 1 / sig_eps
      } else {
        zmat  <- outer(ystar, tau_pad, function(y, t) (t - y))
        inv_s <- 1
      }

      L   <- stats::pnorm(zmat, log.p = TRUE)
      L2  <- L[, -1, drop = FALSE]
      L1  <- L[, -ncol(L), drop = FALSE]
      probs_j <- exp(L2) * (-expm1(L1 - L2))
      mu_vec  <- as.numeric(probs_j %*% scores)

      if (!compute_se) {
        se_mu <- rep(NA_real_, length(rows_k))
      } else {
        phi_mat   <- stats::dnorm(zmat)
        phi_inner <- phi_mat[, -1, drop = FALSE]
        phi_prev  <- phi_mat[, -ncol(phi_mat), drop = FALSE]
        B <- as.numeric((phi_inner[, 1:m, drop = FALSE] - phi_prev[, 1:m, drop = FALSE]) %*% matrix(scores, ncol = 1))

        d_nu   <- (-inv_s)       * B
        d_lamk <- (-eta_k*inv_s) * B
        d_tau  <- matrix(0, nrow = length(rows_k), ncol = m - 1L)
        if (m > 1L) {
          score_diff <- scores[1:(m-1)] - scores[2:m]
          d_tau <- sweep(phi_mat[, 2:(ncol(phi_mat)-1), drop = FALSE], 2, score_diff, `*`) * inv_s
        }

        A_mats <- list(); A_ids <- integer(0)
        id_nu     <- id_map[[g]]$id_nu[j]
        id_lamk   <- id_map[[g]]$id_lam[j, k_name]
        ids_tau_all <- id_map[[g]]$id_tau[[j]]
        ids_tau   <- if (length(ids_tau_all)) ids_tau_all[seq_len(min(length(ids_tau_all), m-1))] else integer(0)

        if (id_nu > 0L)   { A_mats[[length(A_mats)+1L]] <- matrix(d_nu,   ncol = 1L); A_ids <- c(A_ids, id_nu) }
        if (id_lamk > 0L) { A_mats[[length(A_mats)+1L]] <- matrix(d_lamk, ncol = 1L); A_ids <- c(A_ids, id_lamk) }
        if (length(ids_tau)) { A_mats[[length(A_mats)+1L]] <- d_tau; A_ids <- c(A_ids, ids_tau) }

        if (identical(other_latents, "mean")) {
          other_r <- setdiff(nz_idx, k)
          if (length(other_r)) {
            add_cols <- list(); add_ids <- integer(0)
            for (r in other_r) {
              id_lr <- id_map[[g]]$id_lam[j, latent_names[r]]
              if (id_lr > 0L) {
                add_cols[[length(add_cols)+1L]] <- (-B*inv_s) * base_eta[ latent_names[r] ]
                add_ids  <- c(add_ids, id_lr)
              }
            }
            if (length(add_ids)) {
              A_mats[[length(A_mats)+1L]] <- do.call(cbind, add_cols)
              A_ids  <- c(A_ids, add_ids)
            }
          }
        }

        if (length(A_ids)) {
          uniq <- unique(A_ids)
          A_raw <- if (length(A_mats)) do.call(cbind, A_mats) else NULL
          if (!is.null(A_raw) && ncol(A_raw) > 0L) {
            gid <- match(A_ids, uniq)
            A <- t(rowsum(t(A_raw), gid, reorder = FALSE))
            Vsub <- .get_Vsub(V, uniq)
            VsubA <- A %*% Vsub
            se_mu <- sqrt(pmax(rowSums(VsubA * A), 0))
          } else {
            se_mu <- rep(0, length(rows_k))
          }
        } else {
          se_mu <- rep(0, length(rows_k))
        }
      }

      base  <- paste0(j, "_", k_name)
      nm_e  <- paste0("m_est_", base)
      nm_l  <- paste0("m_lwr_", base)
      nm_u  <- paste0("m_upr_", base)

      out_chunks[[idx_j]] <- list(
        rows = rows_k, nm_e = nm_e, nm_l = nm_l, nm_u = nm_u,
        est = mu_vec,
        lwr = if (compute_se) mu_vec - zcrit * se_mu else rep(NA_real_, length(rows_k)),
        upr = if (compute_se) mu_vec + zcrit * se_mu else rep(NA_real_, length(rows_k))
      )
    }

    out_chunks
  }

  # -- Execute tasks (sequential or future) ------------------------------------
  parts <- if (plan == "none") {
    lapply(splits, function(ix) lapply(ix, function(ii) compute_one_gk(tasks_gk$g[ii], tasks_gk$k[ii])))
  } else {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      rlang::abort("Parallel plan requested but 'future.apply' is not installed. Install it or set plan='none'.")
    }
    future.apply::future_lapply(splits, function(ix) lapply(ix, function(ii) compute_one_gk(tasks_gk$g[ii], tasks_gk$k[ii])))
  }
  parts <- unlist(parts, recursive = FALSE, use.names = FALSE)

  # -- Materialize outputs -----------------------------------------------------
  out2 <- out
  nm_e_all <- character(0); nm_l_all <- character(0); nm_u_all <- character(0)
  for (chunk in parts) {
    if (is.null(chunk)) next
    for (piece in chunk) {
      if (is.null(piece)) next
      nm_e_all <- c(nm_e_all, piece$nm_e)
      nm_l_all <- c(nm_l_all, piece$nm_l)
      nm_u_all <- c(nm_u_all, piece$nm_u)
    }
  }
  nm_e_all <- unique(nm_e_all); nm_l_all <- unique(nm_l_all); nm_u_all <- unique(nm_u_all)
  for (nm in nm_e_all) if (is.null(out2[[nm]])) out2[[nm]] <- rep(NA_real_, nrow(out2))
  for (nm in nm_l_all) if (is.null(out2[[nm]])) out2[[nm]] <- rep(NA_real_, nrow(out2))
  for (nm in nm_u_all) if (is.null(out2[[nm]])) out2[[nm]] <- rep(NA_real_, nrow(out2))

  use_vctrs <- requireNamespace("vctrs", quietly = TRUE)
  for (chunk in parts) {
    if (is.null(chunk)) next
    for (piece in chunk) {
      if (is.null(piece)) next
      if (use_vctrs) {
        out2[[piece$nm_e]] <- vctrs::vec_assign(out2[[piece$nm_e]], piece$rows, piece$est)
        out2[[piece$nm_l]] <- vctrs::vec_assign(out2[[piece$nm_l]], piece$rows, piece$lwr)
        out2[[piece$nm_u]] <- vctrs::vec_assign(out2[[piece$nm_u]], piece$rows, piece$upr)
      } else {
        out2[[piece$nm_e]][piece$rows] <- piece$est
        out2[[piece$nm_l]][piece$rows] <- piece$lwr
        out2[[piece$nm_u]][piece$rows] <- piece$upr
      }
    }
  }

  # numeric hygiene
  is_num   <- vapply(out2, is.numeric, logical(1L))
  keep_int <- names(out2) %in% c(".gid", ".rid")
  to_dbl   <- is_num & !keep_int
  for (nm in names(out2)[to_dbl]) out2[[nm]] <- as.double(out2[[nm]])

  tibble::as_tibble(out2)
}





