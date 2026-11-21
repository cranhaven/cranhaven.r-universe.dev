.spline_stage_table <- function(object, stage = c("visiting", "observation")) {
  stage <- match.arg(stage)
  native <- object$fit
  nuisance <- native$nuisance_summary
  if (is.null(nuisance)) return(data.frame())
  vp <- nuisance$visiting_process
  op <- nuisance$observation_process
  joint <- native$stacked_sandwich$vcov_joint
  has_joint <- is.matrix(joint) && !is.null(rownames(joint)) && !is.null(colnames(joint))
  dg <- object$diagnostics %||% list()
  fit_regular <- isTRUE(dg$regular %||% dg$covariance_regular %||% FALSE)
  fit_point <- isTRUE(dg$ok %||% TRUE) && isTRUE(dg$converged %||% TRUE) &&
    isTRUE(dg$identified %||% TRUE)
  fit_boundary <- isTRUE(dg$variance_boundary) || isTRUE(dg$frailty_boundary) ||
    isTRUE(dg$observation_variance_boundary)
  scope <- native$stacked_sandwich$scope %||% dg$inference_scope %||% "not computed"
  rows <- list()
  covariance_se <- function(V, key) {
    if (!is.matrix(V) || is.null(rownames(V)) || is.null(colnames(V)) ||
        !key %in% rownames(V) || !key %in% colnames(V)) return(NA_real_)
    v <- V[key, key]
    if (length(v) != 1L || !is.finite(v) || v < 0)
      return(structure(NA_real_, numerical_failure = is.nan(v) || (!is.na(v) && (!is.finite(v) || v < 0))))
    sqrt(v)
  }
  add <- function(parameter, estimate, joint_key, scale = "coefficient", derivative = 1,
                  fallback_se = NA_real_, fallback_scope = "not available", boundary = FALSE) {
    if (!length(estimate)) return(invisible(NULL))
    estimate <- as.numeric(estimate)[1L]
    supplied_se <- if (has_joint) covariance_se(joint, joint_key) else fallback_se
    bad_cov <- isTRUE(attr(supplied_se, 'numerical_failure'))
    se <- if (has_joint) abs(derivative) * supplied_se else as.numeric(supplied_se)[1L]
    if (!length(se)) se <- NA_real_
    source_scope <- if (has_joint) scope else fallback_scope
    type <- if (has_joint) {
      if (isTRUE(native$stacked_sandwich$regular)) "stacked_sandwich" else "stacked_branch_conditional"
    } else if (grepl("conditional", source_scope, fixed = TRUE)) "plug_in_conditional" else "reported"
    point <- fit_point && is.finite(estimate)
    regular <- fit_regular && !isTRUE(boundary)
    conditional <- !isTRUE(boundary) &&
      (isTRUE(dg$conditional_inference_available %||% dg$conditional_inference_valid) ||
       grepl("conditional", source_scope, fixed = TRUE))
    reason <- .inference_boundary_reason(dg)
    if(stage == "visiting") {
      visiting_ok <- if(!is.null(vp$convergence)) isTRUE(vp$convergence == 0L) else
        isTRUE(dg$gamma_converged %||% dg$visiting$converged %||% dg$nuisance_converged %||% TRUE)
      regular <- visiting_ok && !isTRUE(boundary)
      conditional <- FALSE
      source_scope <- if(startsWith(parameter,"gamma."))
        "Stage-1 visiting-regression covariance; independent of downstream variance-face selection" else
        "Stage-1 frailty-variance covariance"
      reason <- if(startsWith(parameter,"gamma.")) "visiting-regression inference is unavailable" else
        "frailty-variance Wald inference is unavailable at a zero variance or unverified frailty branch"
    }
    row <- data.frame(parameter = parameter, stage = stage,
      estimate = estimate, se = se, scale = scale, variance_type = type,
      variance_label = source_scope, point_valid = point, inference_valid = FALSE,
      conditional_inference_valid = FALSE, df = Inf, boundary = isTRUE(boundary),
      parameter_boundary = isTRUE(boundary), fit_boundary = fit_boundary,
      .se_numerical_failure = bad_cov, lower = NA_real_, upper = NA_real_,
      se_source = if (has_joint) joint_key else source_scope, stringsAsFactors = FALSE)
    row <- .inference_reporting_fields(row,regular,conditional,source_scope,reason,object=object)
    row$conditional_inference_valid <- row$conditional_inference_available & row$inference_valid
    rows[[length(rows) + 1L]] <<- row
  }
  if (stage == "visiting") {
    gamma <- vp$gamma_V %||% numeric()
    for (j in seq_along(gamma)) {
      nm <- names(gamma)[j]
      fallback <- if (length(vp$se_gamma) == length(gamma)) vp$se_gamma[j] else NA_real_
      add(paste0("gamma.", nm), gamma[j], paste0("gamma.", nm),
        fallback_se = fallback, fallback_scope = "native visiting-process sandwich")
    }
    if (length(vp$sigma2_zeta))
      add("sigma2_zeta", vp$sigma2_zeta, "sigma2_zeta", scale = "variance",
        fallback_scope = "frailty variance SE not available without the joint spline sandwich",
        boundary = isTRUE(vp$frailty_boundary) || isTRUE(vp$sigma2_zeta == 0))
  } else {
    alpha <- op$alpha_fixed %||% numeric()
    delta <- op$delta_latent_random_link %||% numeric()
    sigma <- op$total_variance %||% op$Sigma_q %||% numeric()
    variance_prefix <- if(!is.null(op$total_variance)) "tau2." else "sigma2_q."
    conditional_V <- op$vcov_conditional
    for (j in seq_along(alpha)) {
      nm <- names(alpha)[j]; key <- paste0("alpha.", nm)
      add(key, alpha[j], key,
        fallback_se = covariance_se(conditional_V, key),
        fallback_scope = "native observation covariance conditional on visiting nuisance")
    }
    for (j in seq_along(delta)) {
      nm <- names(delta)[j]; key <- paste0("delta.", nm)
      add(key, delta[j], key,
        fallback_se = covariance_se(conditional_V, key),
        fallback_scope = "native observation covariance conditional on visiting nuisance")
    }
    for (j in seq_along(sigma)) {
      nm <- names(sigma)[j]; key <- paste0(variance_prefix, nm)
      is_boundary <- isTRUE(sigma[j] == 0) || isTRUE(op$boundary_variance[nm])
      add(key, sigma[j], key,
        scale = "variance",
        fallback_se = covariance_se(conditional_V, key),
        fallback_scope = "native observation covariance conditional on visiting nuisance",
        boundary = is_boundary)
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}
