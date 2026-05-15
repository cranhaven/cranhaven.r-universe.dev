#' Fast augment for continuous indicators (base R only, order-agnostic)
#'
#' Adds predicted values (yhat = nu + Lambda * eta), residuals, optional
#' delta-method SEs/CIs for yhat, and (optionally) factor-score SEs for
#' continuous-only models. Returns `.rid`, `.gid`, `.group`, original
#' lavPredict output (observed vars, factor scores, and  FS SEs), then
#' augmentation columns.
#'
#' @noRd
#' @keywords internal
.augment_continuous <- function(fit,
                                data            = NULL,
                                info            = NULL,
                                yhat            = TRUE,
                                ci              = TRUE,
                                level           = 0.95,
                                resid           = TRUE,
                                se_fs           = TRUE,
                                se_yhat         = TRUE,
                                prefix_yhat     = ".yhat_",
                                prefix_ci       = c(".yhat_lwr_", ".yhat_upr_"),
                                prefix_resid    = ".resid_",
                                prefix_se_fs    = ".se_",
                                prefix_se_yhat  = ".se_yhat_",
                                vcov_type       = NULL) {

  # -- Validate fit (fast fail) ------------------------------------------------
  .assert_lavaan_fit(
    fit,
    require_converged     = TRUE,
    require_meanstructure = TRUE,
    require_latent        = TRUE,
    forbid_multilevel     = TRUE
  )

  # -- Validate prefix_ci ------------------------------------------------------
  if (!is.character(prefix_ci) || length(prefix_ci) != 2L || any(!nzchar(prefix_ci))) {
    stop("`prefix_ci` must be two non-empty strings, e.g., c('.yhat_lwr_', '.yhat_upr_').", call. = FALSE)
  }
  prefix_yhat_lwr <- prefix_ci[1L]
  prefix_yhat_upr <- prefix_ci[2L]

  # -- Model info (optional injection) ----------------------------------------
  if (is.null(info)) info <- model_info(fit)
  ov_all        <- info$observed_variables
  ov_cont       <- info$ov_continuous
  ov_ord        <- info$ov_ordinal
  latent_names  <- info$latent_variables
  n_groups      <- if (is.null(info$n_groups)) 1L else info$n_groups
  group_labels  <- if (!is.null(info$group_labels)) info$group_labels else as.character(seq_len(n_groups))
  J             <- length(ov_cont)  # use consistently

  if (J == 0L) {
    stop("No continuous observed indicators detected; nothing to predict.", call. = FALSE)
  }

  # -- Factor scores + observed vars (one pass, optional FS SEs) --------------
  request_fs_se <- isTRUE(se_fs) && length(ov_ord) == 0L
  if (is.null(data)) {
    data <- lavPredict_parallel(
      fit,
      return_type  = "list",
      se           = request_fs_se,
      prefix_se_fs = prefix_se_fs
    )
  }
  fs_list <- if (is.data.frame(data)) list(data) else data
  if (length(fs_list) != n_groups) {
    stop("lavPredict returned ", length(fs_list), " group table(s), expected ", n_groups, ".", call. = FALSE)
  }

  if (!is.null(names(fs_list))) {
    m <- match(group_labels, names(fs_list))
    if (anyNA(m)) {
      warning("Some `group_labels` not found in lavPredict output; keeping index order.")
    } else {
      fs_list <- fs_list[m]
    }
  } else {
    # Help downstream code by naming the groups canonically
    names(fs_list) <- group_labels
  }

  # -- case.idx for .rid -------------------------------------------------------
  case_idx <- tryCatch(lavaan::lavInspect(fit, "case.idx"), error = function(e) NULL)

  # -- Measurement parameters --------------------------------------------------
  est      <- lavaan::lavInspect(fit, "est")
  est_list <- if (is.list(est) && "lambda" %in% names(est)) list(est) else est

  # -- Do we need yhat internally? --------------------------------------------
  yhat_needed <- isTRUE(yhat) || isTRUE(resid) || isTRUE(ci) || isTRUE(se_yhat)

  # -- Delta-method prerequisites (compute only if needed) ---------------------
  need_dm_glob <- (isTRUE(ci) || isTRUE(se_yhat))
  if (need_dm_glob) {
    V <- tryCatch({
      if (is.null(vcov_type)) lavaan::vcov(fit) else lavaan::vcov(fit, type = vcov_type)
    }, error = function(e) NULL)
    if (is.null(V)) stop("vcov(fit) is not available; cannot compute delta-method for yhat.", call. = FALSE)
    PT          <- lavaan::parTable(fit)
    single_grp  <- (n_groups == 1L)
    zcrit       <- if (isTRUE(ci)) stats::qnorm((1 + level) / 2) else NA_real_

    # helpers to query free parameter ids (0L if fixed)
    get_free_lambda <- function(g, j, k) {
      if (single_grp) { w <- which(PT$op == "=~" & PT$lhs == k & PT$rhs == j) }
      else            { w <- which(PT$group == g & PT$op == "=~" & PT$lhs == k & PT$rhs == j) }
      if (!length(w)) return(0L); PT$free[w[1L]]
    }
    get_free_nu <- function(g, j) {
      if (single_grp) { w <- which(PT$op == "~1" & PT$lhs == j) }
      else            { w <- which(PT$group == g & PT$op == "~1" & PT$lhs == j) }
      if (!length(w)) return(0L); PT$free[w[1L]]
    }
  }

  # -- Accumulator per group ---------------------------------------------------
  out_groups <- vector("list", n_groups)

  # -- Main loop over groups ---------------------------------------------------
  for (g in seq_len(n_groups)) {
    fs_g <- fs_list[[g]]

    # Build .rid, .gid, .group
    rid_g <- tryCatch({ if (is.list(case_idx)) case_idx[[g]] else case_idx }, error = function(e) NULL)
    if (is.null(rid_g) || length(rid_g) != nrow(fs_g)) rid_g <- seq_len(nrow(fs_g))
    base_g <- cbind(
      .rid   = rid_g,
      .gid   = rep.int(g, nrow(fs_g)),
      .group = rep.int(group_labels[g], nrow(fs_g)),
      fs_g,
      stringsAsFactors = FALSE
    )

    # Extract eta (factor scores); be robust to missing columns
    lat_keep <- intersect(latent_names, colnames(fs_g))
    if (!length(lat_keep)) stop("No latent factor-score columns found in `data`.", call. = FALSE)
    eta_g <- as.matrix(fs_g[, lat_keep, drop = FALSE])

    # Align Lambda rows to ov_cont and columns to latent order
    est_g <- est_list[[g]]
    lam_g <- est_g$lambda
    nu_g  <- est_g$nu

    if (!is.null(colnames(lam_g))) {
      # reorder columns to latent_names (some may be absent in eta_g)
      lam_g <- lam_g[, latent_names, drop = FALSE]
    }
    if (!is.null(rownames(lam_g))) {
      lam_g <- lam_g[ov_cont, , drop = FALSE]
    } else {
      row_idx <- match(ov_cont, ov_all)
      lam_g   <- lam_g[row_idx, , drop = FALSE]
    }

    # Align nu to ov_cont
    if (is.null(nu_g) || length(nu_g) == 0L) {
      nu_g <- rep(0, J); names(nu_g) <- ov_cont
    } else if (!is.null(names(nu_g))) {
      nu_g <- nu_g[ov_cont]
    } else {
      row_idx <- match(ov_cont, ov_all)
      nu_g <- nu_g[row_idx]
    }

    # Conform Lambda to eta columns (if some latent scores missing, drop their columns)
    if (!is.null(colnames(lam_g))) {
      keep_cols <- colnames(lam_g) %in% colnames(eta_g)
      lam_use   <- lam_g[, keep_cols, drop = FALSE]
      eta_use   <- eta_g[, colnames(lam_g)[keep_cols], drop = FALSE]
    } else {
      # No colnames on Lambda: assume full match
      lam_use <- lam_g
      eta_use <- eta_g
    }

    N <- nrow(eta_use)

    # yhat (always compute if needed internally)
    yhat_mat  <- if (yhat_needed) (eta_use %*% t(lam_use)) else NULL   # N x J
    if (!is.null(yhat_mat)) {
      yhat_mat <- as.matrix(yhat_mat)
      yhat_mat <- sweep(yhat_mat, 2L, nu_g, `+`) # add intercepts
      colnames(yhat_mat) <- paste0(prefix_yhat, ov_cont)
    }

    # Residuals (robust to missing observed columns in fs_g)
    resid_mat <- NULL
    if (isTRUE(resid)) {
      missing_ov <- setdiff(ov_cont, colnames(fs_g))
      if (length(missing_ov)) {
        # Prepare an NA-filled matrix for all ov_cont
        y_obs <- matrix(NA_real_, nrow(fs_g), J, dimnames = list(NULL, ov_cont))
        present <- intersect(ov_cont, colnames(fs_g))
        if (length(present)) {
          y_obs[, match(present, ov_cont)] <- as.matrix(fs_g[, present, drop = FALSE])
        }
      } else {
        y_obs <- as.matrix(fs_g[, ov_cont, drop = FALSE])
      }
      resid_core <- y_obs - yhat_mat
      colnames(resid_core) <- paste0(prefix_resid, ov_cont)
      resid_mat <- resid_core
    }

    # Delta-method SEs / CIs for yhat
    se_mat <- lwr_mat <- upr_mat <- NULL
    if (need_dm_glob) {
      # pre-create containers
      if (isTRUE(se_yhat)) se_mat <- matrix(NA_real_, N, J, dimnames = list(NULL, paste0(prefix_se_yhat, ov_cont)))
      if (isTRUE(ci))      {
        lwr_mat <- matrix(NA_real_, N, J, dimnames = list(NULL, paste0(prefix_yhat_lwr, ov_cont)))
        upr_mat <- matrix(NA_real_, N, J, dimnames = list(NULL, paste0(prefix_yhat_upr, ov_cont))) }

      # Per-indicator loop (aggregate gradient columns by shared free id)
      for (j_idx in seq_len(J)) {
        j <- ov_cont[j_idx]

        # free parameter ids for this indicator
        lam_ids <- integer(length(latent_names))
        for (k_idx in seq_along(latent_names)) {
          lam_ids[k_idx] <- get_free_lambda(g, j, latent_names[k_idx])
        }
        nu_id   <- get_free_nu(g, j)

        ids <- c(lam_ids[lam_ids > 0L], if (nu_id > 0L) nu_id else integer(0))
        if (!length(ids)) {
          se_row <- numeric(N)
        } else {
          uniq <- unique(ids)

          # Build A with column-collapsing by free-id (cleaner approach)
          A <- matrix(0, nrow = N, ncol = length(uniq))

          has_lam <- lam_ids > 0L
          if (any(has_lam)) {
            # keep only eta columns present in eta_use (robust to missing FS cols)
            lat_present <- match(latent_names, colnames(eta_use), nomatch = 0L)
            sel_idx <- which(has_lam & lat_present > 0L)
            if (length(sel_idx)) {
              A_lam   <- eta_use[, lat_present[sel_idx], drop = FALSE] # N x (#sel)
              col_ids <- lam_ids[sel_idx]                              # length #sel
              map     <- match(col_ids, uniq)                          # (#sel) -> [1..|uniq|]
              # Accumulate columns by id
              for (kk in seq_along(col_ids)) {
                A[, map[kk]] <- A[, map[kk]] + A_lam[, kk]
              }
            }
          }

          if (nu_id > 0L) {
            hit <- match(nu_id, uniq, nomatch = 0L)
            if (hit > 0L) A[, hit] <- A[, hit] + 1
          }

          # --- SAFE VCOV x GRADIENT HANDLING ---------------------------------
          Vsub <- V[uniq, uniq, drop = FALSE]

          # Drop non-finite diagonals (typ. fixed/robust-problematic params)
          diag_ok <- is.finite(diag(Vsub))
          if (!all(diag_ok)) {
            Vsub <- Vsub[diag_ok, diag_ok, drop = FALSE]
            if (ncol(A) > 0L) A <- A[, diag_ok, drop = FALSE]
          }

          # If nothing left after dropping → zero uncertainty contribution
          if (length(Vsub) == 0L || ncol(A) == 0L) {
            se_row <- numeric(N)
          } else {
            # Do not propagate NA from gradients or VCOV
            if (anyNA(A)) A[is.na(A)] <- 0
            if (any(!is.finite(Vsub))) Vsub[!is.finite(Vsub)] <- 0

            se2   <- rowSums((A %*% Vsub) * A)   # g' V g per row
            se_row <- sqrt(pmax(se2, 0))
          }
        }

        if (isTRUE(se_yhat)) se_mat[, j_idx] <- se_row
        if (isTRUE(ci)) {
          mu <- as.numeric(yhat_mat[, j_idx, drop = TRUE])
          lwr_mat[, j_idx] <- mu - zcrit * se_row
          upr_mat[, j_idx] <- mu + zcrit * se_row
        }
      }
    }

    # Compose group output (single cbind; order not enforced)
    parts_g <- list(base_g)
    if (isTRUE(yhat))  parts_g[[length(parts_g) + 1L]] <- yhat_mat
    if (isTRUE(resid)) parts_g[[length(parts_g) + 1L]] <- resid_mat
    if (isTRUE(ci) && !is.null(lwr_mat)) parts_g[[length(parts_g) + 1L]] <- lwr_mat
    if (isTRUE(ci) && !is.null(upr_mat)) parts_g[[length(parts_g) + 1L]] <- upr_mat
    if (isTRUE(se_yhat) && !is.null(se_mat)) parts_g[[length(parts_g) + 1L]] <- se_mat

    out_groups[[g]] <- do.call(cbind, parts_g)
  } # end groups

  # -- Final rbind and numeric casting ----------------------------------------
  out <- do.call(rbind, out_groups)

  # Cast numeric columns to double (keep .rid/.gid integers if you prefer)
  is_num <- vapply(out, is.numeric, logical(1L))
  keep_int <- names(out) %in% c(".rid", ".gid")
  to_double <- is_num & !keep_int
  if (any(to_double)) {
    for (nm in names(out)[to_double]) out[[nm]] <- as.double(out[[nm]])
  }

  # Return as tibble-like data.frame (no dplyr dependency needed)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
