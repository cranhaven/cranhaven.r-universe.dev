#' @noRd
#' @keywords internal
.augment_ordinal <- function(fit,
                             data            = NULL,
                             info            = NULL,
                             ystar           = TRUE,
                             yhat            = TRUE,
                             pr              = TRUE,
                             ci              = TRUE,
                             level           = 0.95,
                             resid           = TRUE,
                             se_yhat         = TRUE,
                             prefix_ystar    = ".ystar_",
                             prefix_yhat     = ".yhat_",
                             prefix_pr       = ".pr_",
                             prefix_ci       = c(".yhat_lwr_", ".yhat_upr_"),
                             prefix_resid    = ".resid_",
                             prefix_se_yhat  = ".se_yhat_",
                             sep             = "__",
                             vcov_type       = NULL) {

  # -- Assertions (fail fast) --------------------------------------------------
  .assert_lavaan_fit(
    fit,
    require_converged     = TRUE,
    require_latent        = TRUE,
    require_meanstructure = NA,
    require_measurement   = "lambda+nu",
    forbid_multilevel     = TRUE
  )

  if (!is.character(prefix_ci) || length(prefix_ci) != 2L || any(!nzchar(prefix_ci))) {
    stop("`prefix_ci` must be two non-empty strings, e.g., c('.yhat_lwr_', '.yhat_upr_').", call. = FALSE)
  }

  # -- Model info (optional injection) ----------------------------------------
  if (is.null(info)) info <- model_info(fit)
  ov_all       <- info$observed_variables
  ov_ord       <- info$ov_ordinal
  n_groups     <- info$n_groups %||% 1L
  group_labels <- info$group_labels %||% as.character(seq_len(n_groups))
  latent_names <- info$latent_variables
  param        <- info$parameterization %||% "delta"

  # No ordinal indicators -> nothing to do
  if (length(ov_ord) == 0L) {
    stop("No ordinal indicators detected; nothing to augment.", call. = FALSE)
  }

  # -- lavPredict output (optional injection) ---------------------------------
  if (is.null(data)) data <- lavPredict_parallel(fit, return_type = "list")
  fs_list <- if (is.data.frame(data)) list(data) else data
  if (length(fs_list) != n_groups) {
    stop("Expected ", n_groups, " group table(s) from lavPredict, got ", length(fs_list), ".", call. = FALSE)
  }

  if (!is.null(names(fs_list))) {
    m <- match(group_labels, names(fs_list))
    if (anyNA(m)) {
      warning("Some `group_labels` not found in lavPredict output; keeping index order.")
    } else {
      fs_list <- fs_list[m]
    }
  } else {
    names(fs_list) <- group_labels
  }

  # -- case.idx for .rid (single call) ----------------------------------------
  case_idx <- tryCatch(lavaan::lavInspect(fit, "case.idx"), error = function(e) NULL)

  # -- Measurement & thresholds (single calls) ---------------------------------
  est      <- lavaan::lavInspect(fit, "est")
  est_list <- if (is.list(est) && "lambda" %in% names(est)) list(est) else est

  th_raw   <- tryCatch(lavaan::lavInspect(fit, "th"), error = function(e) NULL)
  if (is.null(th_raw)) {
    th_list <- replicate(n_groups, stats::setNames(numeric(0), character(0)), simplify = FALSE)
  } else if (is.list(th_raw)) {
    th_list <- th_raw
  } else {
    th_list <- list(th_raw)
  }
  if (length(th_list) == 1L && n_groups > 1L) th_list <- rep(th_list, n_groups)
  names(th_list) <- group_labels

  # -- Delta-method prerequisites (compute only if needed) ---------------------
  need_dm_glob <- isTRUE(yhat) && (isTRUE(se_yhat) || isTRUE(ci))
  coef_names   <- if (need_dm_glob) names(lavaan::coef(fit)) else NULL

  # vcov_type semantics aligned with augment_continuous():
  # - NULL  -> use default vcov(fit)
  # - char  -> pass through to vcov(fit, type = <that>)
  V_full <- if (need_dm_glob) {
    tryCatch({
      if (is.null(vcov_type)) {
        lavaan::vcov(fit)
      } else {
        if (!is.character(vcov_type) || length(vcov_type) != 1L || !nzchar(vcov_type)) {
          stop("`vcov_type` must be NULL or a non-empty character scalar.", call. = FALSE)
        }
        lavaan::vcov(fit, type = vcov_type)
      }
    }, error = function(e) {
      stop("vcov(fit", if (!is.null(vcov_type)) paste0(", type = '", vcov_type, "'"),
           ") is not available; cannot compute delta-method for E[Y].", call. = FALSE)
    })
  } else NULL

  # if VCOV is unavailable because user set ci/se_yhat but vcov failed, we already stopped.
  zcrit <- if (isTRUE(ci)) stats::qnorm(1 - (1 - level)/2) else NA_real_



  # -- Helpers (lightweight) ---------------------------------------------------
  align_Lambda_nu <- function(lam_g, nu_g, ov_subset, ov_all, latent_order) {
    # Reorder Lambda columns to latent order; rows to ov_subset
    if (!is.null(colnames(lam_g))) lam_g <- lam_g[, latent_order, drop = FALSE]
    if (!is.null(rownames(lam_g))) {
      lam_g <- lam_g[ov_subset, , drop = FALSE]
    } else {
      row_idx <- match(ov_subset, ov_all); lam_g <- lam_g[row_idx, , drop = FALSE]
    }
    # Intercepts (nu) aligned to ov_subset
    if (is.null(nu_g) || length(nu_g) == 0L) {
      nu_g <- rep(0, length(ov_subset)); names(nu_g) <- ov_subset
    } else if (!is.null(names(nu_g))) {
      nu_g <- nu_g[ov_subset]
    } else {
      row_idx <- match(ov_subset, ov_all); nu_g <- nu_g[row_idx]
    }
    list(lambda = lam_g, nu = nu_g)
  }
  get_tau_for <- function(th_vec, j) {
    nm <- names(th_vec)
    if (is.null(nm)) return(numeric(0))
    sel <- startsWith(nm, paste0(j, "|"))
    tau <- th_vec[sel]
    if (!length(tau)) return(numeric(0))
    idx <- suppressWarnings(as.integer(gsub("^.*\\|t(\\d+)$", "\\1", names(tau))))
    idx[is.na(idx)] <- seq_along(tau)
    tau[order(idx)]
  }
  levels_for_item <- function(df, th_vec, j) {
    if (!is.null(df) && j %in% names(df)) {
      yo <- df[[j]]
      if (is.factor(yo)) return(levels(yo))
      lev <- sort(unique(yo)); return(as.character(lev))
    } else {
      tau_j <- get_tau_for(th_vec, j)
      if (!length(tau_j)) return(character(0))
      m <- length(tau_j) + 1L
      as.character(seq_len(m))
    }
  }
  needs_double_from_special <- function(x) inherits(x, "lvn.mtrx") || inherits(x, "lavaan.matrix")

  # -- Accumulators per group --------------------------------------------------
  out_groups <- vector("list", n_groups)

  # -- Main group loop ---------------------------------------------------------
  for (g in seq_len(n_groups)) {
    est_g <- est_list[[g]]
    lam_g <- est_g$lambda
    nu_g  <- est_g$nu %||% numeric(0)

    # Align Lambda/nu to ordinal items
    al <- align_Lambda_nu(lam_g, nu_g, ov_subset = ov_ord, ov_all = ov_all, latent_order = latent_names)

    # Factor scores (eta) matrix for this group (N x q)
    fs_g <- fs_list[[g]]
    lat_keep <- intersect(latent_names, colnames(fs_g))
    eta_g <- as.matrix(fs_g[, lat_keep, drop = FALSE])

    # y* = eta %*% t(Lambda) + nu  (N x p_ord)
    ystar_g <- eta_g %*% t(al$lambda)
    ystar_g <- sweep(ystar_g, 2L, al$nu, `+`)
    colnames(ystar_g) <- ov_ord

    # Residual SDs for theta parameterization (vector per item)
    sig_eps <- NULL
    if (identical(param, "theta") && (isTRUE(pr) || isTRUE(yhat) || isTRUE(resid))) {
      theta_g <- est_g$theta
      if (!is.null(theta_g)) {
        if (!is.null(rownames(theta_g))) {
          sig_eps <- sqrt(diag(theta_g)[ov_ord])
        } else {
          idx <- match(ov_ord, ov_all); sig_eps <- sqrt(diag(theta_g)[idx])
        }
        bad <- !is.finite(sig_eps) | sig_eps <= 0
        if (any(bad)) sig_eps[bad] <- 1
      } else sig_eps <- rep(1, length(ov_ord))
    }

    N <- nrow(ystar_g); P <- ncol(ystar_g)

    # Preallocate matrices (only if needed)
    ystar_mat <- if (isTRUE(ystar)) ystar_g else NULL

    yhat_mat  <- if (isTRUE(yhat)) {
      matrix(NA_real_, N, P, dimnames = list(NULL, paste0(prefix_yhat, ov_ord)))
    } else NULL

    se_mat    <- if (isTRUE(yhat) && isTRUE(se_yhat) && need_dm_glob) {
      matrix(NA_real_, N, P, dimnames = list(NULL, paste0(prefix_se_yhat, ov_ord)))
    } else NULL

    lwr_mat   <- if (isTRUE(yhat) && isTRUE(ci) && need_dm_glob) {
      matrix(NA_real_, N, P, dimnames = list(NULL, paste0(prefix_ci[[1L]], ov_ord)))
    } else NULL

    upr_mat   <- if (isTRUE(yhat) && isTRUE(ci) && need_dm_glob) {
      matrix(NA_real_, N, P, dimnames = list(NULL, paste0(prefix_ci[[2L]], ov_ord)))
    } else NULL

    resid_mat <- if (isTRUE(resid)) {
      matrix(NA_real_, N, P, dimnames = list(NULL, paste0(prefix_resid, ov_ord)))
    } else NULL

    pr_list   <- if (isTRUE(pr)) vector("list", P) else NULL

    # Lambda columns used in derivatives (match to latent_names)
    lam_cols <- if (!is.null(colnames(al$lambda))) colnames(al$lambda) else latent_names
    eta_mat_dm <- eta_g[, lam_cols, drop = FALSE]

    # Per-item loop
    for (jj in seq_len(P)) {
      j       <- ov_ord[jj]
      ystar_j <- ystar_g[, jj]

      lev   <- levels_for_item(fs_g, th_list[[g]], j)
      tau_j <- as.numeric(get_tau_for(th_list[[g]], j))
      if (!length(lev) || !length(tau_j)) next
      if (length(lev) != (length(tau_j) + 1L)) lev <- as.character(seq_len(length(tau_j) + 1L))

      m       <- length(lev)
      tau_pad <- c(-Inf, tau_j, Inf)

      # z-matrix, CDF and category probabilities
      if (identical(param, "theta")) {
        se_j <- if (is.null(sig_eps)) 1 else sig_eps[jj]
        zmat <- outer(ystar_j, tau_pad, function(y, t) (t - y) / se_j)
      } else {
        zmat <- outer(ystar_j, tau_pad, function(y, t) (t - y))
      }
      Phi     <- stats::pnorm(zmat)
      probs_j <- Phi[, -1, drop = FALSE] - Phi[, -ncol(Phi), drop = FALSE]  # N x m

      # Expected score and residuals
      if (isTRUE(yhat) || isTRUE(resid)) {
        sc <- suppressWarnings(as.numeric(lev))
        if (any(is.na(sc))) sc <- seq_len(m)
        yexp_j <- as.numeric(probs_j %*% sc)
        if (isTRUE(yhat))  yhat_mat[, jj] <- yexp_j

        if (isTRUE(resid)) {
          yobs <- fs_g[[j]]
          if (is.factor(yobs)) {
            yobs_num <- match(as.character(yobs), lev)
          } else {
            idx_obs <- match(as.character(yobs), lev)
            if (any(is.na(idx_obs))) {
              idx2 <- match(suppressWarnings(as.numeric(yobs)), suppressWarnings(as.numeric(lev)))
              idx_obs[is.na(idx_obs)] <- idx2[is.na(idx_obs)]
            }
            yobs_num <- idx_obs
          }
          resid_mat[, jj] <- as.numeric(yobs_num) - yexp_j
        }
      }

      # Delta-method for SE/CI of E[Y] (only if needed)
      if (need_dm_glob && isTRUE(yhat) && (isTRUE(se_yhat) || isTRUE(ci))) {
        phi_mat   <- stats::dnorm(zmat)
        phi_inner <- phi_mat[, -1, drop = FALSE]
        phi_prev  <- phi_mat[, -ncol(phi_mat), drop = FALSE]

        B <- as.numeric((phi_inner[, 1:m, drop = FALSE] - phi_prev[, 1:m, drop = FALSE]) %*% matrix(sc, ncol = 1))

        # dmu/d nu
        if (identical(param, "theta")) {
          se_j <- if (is.null(sig_eps)) 1 else sig_eps[jj]
          d_nu <- (-1 / se_j) * B
        } else {
          d_nu <- (-1) * B
        }

        # dmu/d lambda_jr
        if (identical(param, "theta")) {
          dz_lam_mat <- -eta_mat_dm / (if (is.null(sig_eps)) 1 else sig_eps[jj])
        } else {
          dz_lam_mat <- -eta_mat_dm
        }
        d_lam_mat <- dz_lam_mat * B

        # dmu/d tau_{jr}
        s_diff    <- sc[1:(m - 1)] - sc[2:m]
        phi_r_mat <- phi_mat[, 1 + seq_len(m - 1), drop = FALSE]
        if (identical(param, "theta")) {
          se_j <- if (is.null(sig_eps)) 1 else sig_eps[jj]
          d_tau_mat <- sweep(phi_r_mat, 2L, (s_diff / se_j), `*`)
        } else {
          d_tau_mat <- sweep(phi_r_mat, 2L, s_diff, `*`)
        }

        # dmu/d theta_jj (theta only)
        if (identical(param, "theta")) {
          se_j_local <- if (is.null(sig_eps)) 1 else sig_eps[jj]
          dz_sigma   <- -(zmat / se_j_local)
          A <- phi_inner * dz_sigma[, -1, drop = FALSE]
          C <- phi_prev  * dz_sigma[, -ncol(dz_sigma), drop = FALSE]
          d_sigma <- as.numeric((A[, 1:m, drop = FALSE] - C[, 1:m, drop = FALSE]) %*% matrix(sc, ncol = 1))
          d_var   <- d_sigma * (1 / (2 * se_j_local))
        }

        # Build gradient G against the subset of coefficients present
        th_names  <- if (m > 1L) paste0(j, "|t", seq_len(m - 1L)) else character(0L)
        nu_name   <- {
          nm <- paste0("nu[", j, "]")
          if (nm %in% coef_names) nm else {
            alt <- paste0(j, "~1"); if (alt %in% coef_names) alt else NA_character_
          }
        }
        lam_names <- paste0(lam_cols, "=~", j)
        var_name  <- if (identical(param, "theta")) paste0(j, "~~", j) else NA_character_

        keep <- c(th_names, nu_name, lam_names, var_name)
        keep <- keep[!is.na(keep) & keep %in% coef_names]

        if (length(keep)) {
          idx  <- match(keep, coef_names)
          Vsub <- V_full[idx, idx, drop = FALSE]

          G_parts <- list()
          p_idx <- 0L

          if (length(th_names)) {
            sel <- th_names %in% keep
            if (any(sel)) { p_idx <- p_idx + 1L; G_parts[[p_idx]] <- d_tau_mat[, sel, drop = FALSE] }
          }
          if (!is.na(nu_name) && nu_name %in% keep) {
            p_idx <- p_idx + 1L
            M <- matrix(d_nu, ncol = 1); colnames(M) <- nu_name
            G_parts[[p_idx]] <- M
          }
          lam_keep <- lam_names[lam_names %in% keep]
          if (length(lam_keep)) {
            # extract latent names from "latent=~item"
            lam_keep_lat <- sub("=~.*$", "", lam_keep)
            sel <- lam_cols %in% lam_keep_lat
            if (any(sel)) {
              M <- d_lam_mat[, sel, drop = FALSE]
              # keep column order consistent with lam_keep (by latent order)
              if (!is.null(colnames(M))) {
                col_order <- match(lam_keep_lat[lam_keep_lat %in% colnames(M)], colnames(M))
                col_order <- col_order[!is.na(col_order)]
                if (length(col_order)) M <- M[, col_order, drop = FALSE]
              }
              colnames(M) <- lam_keep
              # append to gradient parts
              # (use the same accumulator variable names you use above: p_idx/G_parts)
              p_idx <- p_idx + 1L
              G_parts[[p_idx]] <- M
            }
          }
          if (identical(param, "theta") && !is.na(var_name) && var_name %in% keep) {
            p_idx <- p_idx + 1L
            M <- matrix(d_var, ncol = 1); colnames(M) <- var_name
            G_parts[[p_idx]] <- M
          }

          if (p_idx > 0L) {
            G  <- do.call(cbind, G_parts)
            GV <- G %*% Vsub
            se_vec <- sqrt(rowSums(GV * G))
          } else {
            se_vec <- rep(NA_real_, N)
          }
        } else {
          se_vec <- rep(NA_real_, N)
        }

        if (!is.null(se_mat))  se_mat[, jj]  <- se_vec
        if (!is.null(lwr_mat)) lwr_mat[, jj] <- yhat_mat[, jj] - zcrit * se_vec
        if (!is.null(upr_mat)) upr_mat[, jj] <- yhat_mat[, jj] + zcrit * se_vec
      }

      # Store per-category probabilities for this item (column names once)
      if (isTRUE(pr)) {
        pr_colnames <- paste0(prefix_pr, make.names(lev, allow_ = TRUE), sep, j)
        colnames(probs_j) <- pr_colnames
        pr_list[[jj]] <- probs_j
      }
    } # end items loop

    # -- Build group output (single cbind) ------------------------------------
    # Base: .rid, .gid, .group + original fs table (observed vars + factor scores)
    rid_g <- tryCatch({ if (is.list(case_idx)) case_idx[[g]] else case_idx }, error = function(e) NULL)
    if (is.null(rid_g) || length(rid_g) != nrow(fs_g)) rid_g <- seq_len(nrow(fs_g))
    base_g <- cbind(.rid = rid_g, .gid = rep.int(g, nrow(fs_g)), .group = rep.int(group_labels[g], nrow(fs_g)), fs_g, stringsAsFactors = FALSE)

    parts_g <- list(base_g)
    if (!is.null(ystar_mat)) {
      colnames(ystar_mat) <- paste0(prefix_ystar, colnames(ystar_mat))
      parts_g[[length(parts_g) + 1L]] <- ystar_mat
    }
    if (!is.null(yhat_mat) || !is.null(lwr_mat) || !is.null(upr_mat) || !is.null(resid_mat)) {
      if (!is.null(yhat_mat))  parts_g[[length(parts_g) + 1L]] <- yhat_mat
      if (!is.null(lwr_mat))   parts_g[[length(parts_g) + 1L]] <- lwr_mat
      if (!is.null(upr_mat))   parts_g[[length(parts_g) + 1L]] <- upr_mat
      if (!is.null(resid_mat)) parts_g[[length(parts_g) + 1L]] <- resid_mat
      if (!is.null(se_mat))    parts_g[[length(parts_g) + 1L]] <- se_mat
    }
    if (isTRUE(pr)) {
      pr_bind <- pr_list[!vapply(pr_list, is.null, TRUE)]
      if (length(pr_bind)) {
        parts_g[[length(parts_g) + 1L]] <- do.call(cbind, pr_bind)
      }
    }

    out_groups[[g]] <- do.call(cbind, parts_g)
  } # end group loop

  # -- Final rbind and numeric casting ----------------------------------------
  out <- do.call(rbind, out_groups)

  # Coerce special lavaan classes to double; then ensure numeric -> double
  is_special <- vapply(out, needs_double_from_special, logical(1L))
  if (any(is_special)) {
    for (nm in names(out)[is_special]) out[[nm]] <- as.double(out[[nm]])
  }
  is_num <- vapply(out, is.numeric, logical(1L))
  keep_int <- names(out) %in% c(".rid", ".gid")
  to_double <- is_num & !keep_int
  if (any(to_double)) for (nm in names(out)[to_double]) out[[nm]] <- as.double(out[[nm]])

  # Return as data.frame (optionally tibble-like class if you want)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
