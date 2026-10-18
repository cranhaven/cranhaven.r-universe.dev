#' @importFrom stats binomial glm qnorm rnorm terms.formula update.formula var
#' @importFrom Matrix spMatrix
#' @importFrom methods as
#' @importFrom stats as.formula
#' @importFrom parallel parLapply
#' @importFrom MASS ginv
is_cluster_active <- function(clust) {
  if (is.null(clust)) {
    return(FALSE)
  }

  # Try to evaluate a simple expression on the cluster
  test_result <- tryCatch(
    {
      parallel::clusterEvalQ(clust, {
        1 + 1
      })
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  return(test_result)
}

#' @title Set Control Parameters for iglm Estimation
#'
#' @description
#' Create a list of control parameters for the `iglm` estimation algorithm.
#'
#' @param estimate_model (logical) If `TRUE` (default), the main model parameters
#'   are estimated. If `FALSE`, estimation is skipped and only the preprocessing is done.
#' @param display_progress (logical) If `TRUE`, display progress messages or
#'   a progress bar during estimation. Default is `FALSE`.
#' @param return_samples (logical). If \code{TRUE} (default), return simulated network/attribute
#'   samples (i.e., \code{iglm.data} objects) generated during estimation (if applicable).
#' @param return_z (logical). If \code{TRUE}, return the change statistics for the \code{z} network. Default is \code{FALSE}.
#' @param offset_nonoverlap (numeric) A value added to the linear predictor for
#'   dyads not in the 'overlap' set. Default is `0`.
#' @param var_method (string) Method for variance estimation. Options are "Mean-value" (default), "Godambe", and "Hessian".
#'   The mean-value version is described in Section 3.3 of Fritz et al. (2025), the Godambe method is described in Schmid and Hunter (2023), and the "Hessian" option just assumes that the pseudo likelihood is the correct likelihood.
#' @param exact (logical) If `TRUE`, the pseudo Fisher information is calculated exact for assessing the uncertainty of the estimates. Default is `FALSE`.
#' @param non_stop (logical) If `TRUE`, the estimation algorithm continues until
#'   `max_it` iterations, ignoring the `tol` convergence criterion. Default is `FALSE`.
#' @param tol (numeric) The tolerance level for convergence. The estimation stops
#'   when the change in coefficients between iterations is less than `tol`.
#'   Default is `0.001`.
#' @param max_it (integer) The maximum number of iterations for the estimation
#'   algorithm. Default is `100`.
#' @param return_y (logical). If \code{TRUE}, return the change statistics for the \code{y} attribute Default is \code{FALSE}.
#' @param return_x (logical). If \code{TRUE}, return the change statistics for the \code{x} attribute Default is \code{FALSE}.
#'   from samples. Default is `FALSE`. (Note: `return_samples=TRUE` likely implies this).
#' @param accelerated (logical) If `TRUE` (default), an accelerated MM algorithm is used based on a Quasi Newton scheme described in the Supplemental Material of Fritz et al (2025).
#' @references
#' Fritz, C., Schweinberger, M. , Bhadra S., and D. R. Hunter (2025). A Regression Framework for Studying Relationships among Attributes under Network Interference. Journal of the American Statistical Association, to appear.
#'
#' Schmid, C.S. and D. R. Hunter (2023). Computing Pseudolikelihood Estimators for Exponential-Family Random Graph Models. Journal of Data Science
#'
#' @return A list object of class `"control.iglm"` containing the specified
#'   control parameters.
#' @export
control.iglm <- function(estimate_model = TRUE,
                         display_progress = FALSE,
                         return_samples = TRUE,
                         offset_nonoverlap = 0,
                         var_method = "Mean-value",
                         non_stop = FALSE,
                         tol = 0.001,
                         max_it = 100,
                         return_x = FALSE,
                         return_y = FALSE,
                         return_z = FALSE,
                         accelerated = TRUE,
                         exact = TRUE) {
  if (!var_method %in% c("Godambe", "Mean-value", "Hessian")) {
    stop("var_method must be one of 'Godambe', 'Mean-value', or 'Hessian'")
  }
  if (!is.numeric(max_it) || length(max_it) != 1 || is.na(max_it) || !is.finite(max_it) || max_it <= 0 || max_it %% 1 != 0) {
    stop("`max_it` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1 || is.na(tol) || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(offset_nonoverlap) || length(offset_nonoverlap) != 1 || is.na(offset_nonoverlap) || !is.finite(offset_nonoverlap)) {
    stop("`offset_nonoverlap` must be a single numeric value.", call. = FALSE)
  }
  if (var_method == "Mean-value") {
    updated_uncertainty <- TRUE
  } else {
    updated_uncertainty <- FALSE
  }
  if (var_method == "Hessian") {
    var <- FALSE
  } else {
    var <- TRUE
  }

  res <- list(
    estimate_model = estimate_model,
    display_progress = display_progress,
    offset_nonoverlap = offset_nonoverlap, var = var,
    max_it = max_it, non_stop = non_stop,
    tol = tol, return_samples = return_samples,
    return_x = return_x, return_y = return_y, return_z = return_z,
    accelerated = accelerated, exact = exact,
    updated_uncertainty = updated_uncertainty,
    var_method = var_method
  )
  class(res) <- "control.iglm"
  return(res)
}
#' @export
#' @method print control.iglm
print.control.iglm <- function(x, ...) {
  # Header
  cat("Control Parameters for 'iglm'\n")

  # --- Group 1: Model & Estimation ---
  cat("\n--- Model Estimation ---\n")
  # We find the longest name ("updated_uncertainty", 20 chars) and pad to 22
  # to create an aligned "Key : Value" format.
  cat(sprintf("  %-22s: %s\n", "estimate model", x$estimate_model))
  cat(sprintf("  %-22s: %s\n", "variance method", x$var_method))
  cat(sprintf("  %-22s: %s\n", "exact", x$exact))

  # --- Group 2: Convergence Control ---
  cat("\n--- Convergence Control ---\n")
  cat(sprintf("  %-22s: %s\n", "tol", x$tol))
  cat(sprintf("  %-22s: %s\n", "max_it", x$max_it))
  cat(sprintf("  %-22s: %s\n", "non_stop", x$non_stop))
  cat(sprintf("  %-22s: %s\n", "accelerated", x$accelerated))
  cat(sprintf("  %-22s: %s\n", "offset_nonoverlap", x$offset_nonoverlap))

  # --- Group 3: Output & Verbosity ---
  cat("\n--- Output Control ---\n")
  cat(sprintf("  %-22s: %s\n", "display_progress", x$display_progress))
  cat(sprintf("  %-22s: %s\n", "return_samples", x$return_samples))
  cat(sprintf("  %-22s: %s\n", "return_x", x$return_x))
  cat(sprintf("  %-22s: %s\n", "return_y", x$return_y))
  cat(sprintf("  %-22s: %s\n", "return_z", x$return_z))


  invisible(x)
}

estimate_xyz <- function(formula, preprocessed, control = control.iglm(),
                         sampler = sampler.iglm(),
                         beg_coef = NULL,
                         beg_coef_degrees = NULL,
                         nonoverlap_random = TRUE,
                         data_object,
                         start = 0) {
  # if(data_object$fix_z & preprocessed$includes_degrees){
  #   warning("fix_z = TRUE is incompatible with models including degree parameters.
  #           Setting includes_degrees = FALSE.")
  #   preprocessed$includes_degrees <- FALSE
  # }

  return_preprocess <- control$return_x + control$return_y + control$return_z > 0
  if (preprocessed$includes_degrees) {
    n_units <- length(data_object$x_attribute)
    if (is.null(beg_coef)) {
      coef_tmp <- rep(0, length(preprocessed$term_names))
    } else {
      coef_tmp <- as.vector(beg_coef)
    }

    if (is.null(beg_coef_degrees)) {
      if (data_object$directed) {
        coef_tmp_degrees <- rep(0, n_units * 2)
      } else {
        coef_tmp_degrees <- rep(0, n_units)
      }
    } else {
      coef_tmp_degrees <- as.vector(beg_coef_degrees)
    }
    if (control$estimate_model) {
      # browser()
      res <- outerloop_estimation_pl(
        coef = coef_tmp, coef_degrees = coef_tmp_degrees,
        data_object$z_network,
        data_object$x_attribute,
        data_object$y_attribute,
        neighborhood = data_object$neighborhood,
        overlap = data_object$overlap,
        directed = data_object$directed,
        terms = preprocessed$term_names,
        max_iteration_outer = control$max_it,
        max_iteration_inner_degrees = 1,
        max_iteration_inner_nondegrees = 1,
        data_list = preprocessed$data_list,
        type_list = preprocessed$type_list,
        nonoverlap_random = nonoverlap_random,
        display_progress = control$display_progress,
        tol = control$tol,
        non_stop = control$non_stop,
        offset_nonoverlap = control$offset_nonoverlap,
        var = control$var,
        accelerated = control$accelerated,
        fix_x = data_object$fix_x,
        family_x = data_object$family_x,
        family_y = data_object$family_y,
        attr_x_scale = data_object$scale_x,
        attr_y_scale = data_object$scale_y,
        start = start
      )

      ind_droped <- (as.vector(res$where_wrong) + 1)
      if (length(ind_droped) > 0) {
        preprocessed$term_names <- preprocessed$term_names[-ind_droped]
        preprocessed$coef_names <- preprocessed$coef_names[-ind_droped]
        preprocessed$data_list <- preprocessed$data_list[-ind_droped]
        preprocessed$type_list <- preprocessed$type_list[-ind_droped]
      }
      if (control$var) {
        if (sampler$n_simulation <= 1) {
          stop("Variance estimation requested but sampler has less than 1 simulation. Variance cannot be estimated.")
        }
        if (control$display_progress) {
          cat("Starting with samples to estimate uncertainty \n")
        }

        if (is.null(sampler)) {
          sampler <- sampler.iglm()
          # if no specifications of the sampler are provided use the default one
        }
        # browser()
        if (!is_cluster_active(sampler$cluster)) {
          sampler$deactive_cluster()
        }
        if (is.null(sampler$cluster)) {
          # browser()
          variability_simulations <- xyz_approximate_variability(
            coef = res$coefficients_nondegrees,
            coef_degrees = res$coefficients_degrees,
            terms = preprocessed$term_names,
            n_units = n_units,
            z_network = data_object$z_network,
            neighborhood = data_object$neighborhood,
            overlap = data_object$overlap,
            x_attribute = data_object$x_attribute,
            y_attribute = data_object$y_attribute,
            init_empty = sampler$init_empty,
            directed = data_object$directed,
            nonoverlap_random = nonoverlap_random,
            data_list = preprocessed$data_list,
            type_list = preprocessed$type_list,
            n_proposals_x = sampler$sampler_x$n_proposals,
            n_proposals_y = sampler$sampler_y$n_proposals,
            tnt = sampler$sampler_z$tnt,
            seed = sampler$seed,
            n_proposals_z = sampler$sampler_z$n_proposals,
            n_burn_in = sampler$n_burn_in,
            n_simulation = sampler$n_simulation,
            display_progress = control$display_progress,
            degrees = TRUE,
            offset_nonoverlap = control$offset_nonoverlap,
            return_samples = control$return_samples,
            fix_x = data_object$fix_x,
            fix_z = data_object$fix_z,
            updated_uncertainty = control$updated_uncertainty,
            exact = control$exact,
            family_x = data_object$family_x,
            family_y = data_object$family_y,
            attr_x_scale = data_object$scale_x,
            attr_y_scale = data_object$scale_y
          )


          if (control$return_samples) {
            res$simulations <- list(
              simulation_x_attributes = variability_simulations$simulation_x_attributes,
              simulation_y_attributes = variability_simulations$simulation_y_attributes,
              simulation_z_networks = variability_simulations$simulation_z_networks
            )
          }
          res$stats <- variability_simulations$stats
        } else {
          tmp_split <- suppressWarnings(split(1:sampler$n_simulation, seq_along(sampler$cluster)))
          if (control$display_progress) {
            cat("Starting with parallel simulations")
          }
          # browser()
          res_parallel <- parLapply(cl = sampler$cluster, X = tmp_split, fun = function(x, preprocessed,
                                                                                        n_units, res, control, term_names) {
            xyz_approximate_variability(
              coef = res$coefficients_nondegrees,
              coef_degrees = res$coefficients_degrees,
              terms = preprocessed$term_names,
              n_units = n_units,
              z_network = data_object$z_network,
              neighborhood = data_object$neighborhood,
              overlap = data_object$overlap,
              x_attribute = data_object$x_attribute,
              y_attribute = data_object$y_attribute,
              init_empty = sampler$init_empty,
              nonoverlap_random = nonoverlap_random,
              directed = data_object$directed,
              tnt = sampler$sampler_z$tnt,
              data_list = preprocessed$data_list,
              type_list = preprocessed$type_list,
              n_proposals_x = sampler$sampler_x$n_proposals,
              seed = sampler$seed + min(x),
              n_proposals_y = sampler$sampler_y$n_proposals,
              n_proposals_z = sampler$sampler_z$n_proposals,
              n_burn_in = sampler$n_burn_in,
              n_simulation = length(x),
              display_progress = control$display_progress,
              degrees = TRUE,
              offset_nonoverlap = control$offset_nonoverlap,
              return_samples = control$return_samples,
              fix_x = data_object$fix_x,
              fix_z = data_object$fix_z,
              updated_uncertainty = control$updated_uncertainty,
              exact = control$exact,
              family_x = data_object$family_x,
              family_y = data_object$family_y,
              attr_x_scale = data_object$scale_x,
              attr_y_scale = data_object$scale_y
            )
          }, preprocessed = preprocessed, n_units = n_units, res = res, control = control, term_names = preprocessed$term_names)


          variability_simulations <- list()
          variability_simulations$gradients_nondegrees <- do.call(rbind, lapply(res_parallel, function(x) {
            x$gradients_nondegrees
          }))
          variability_simulations$gradients_degrees <- do.call(rbind, lapply(res_parallel, function(x) {
            x$gradients_degrees
          }))

          if (control$return_samples) {
            res$simulations <- list(
              simulation_x_attributes = unlist(lapply(res_parallel, function(x) {
                x$simulation_x_attributes
              }), recursive = F),
              simulation_y_attributes = unlist(lapply(res_parallel, function(x) {
                x$simulation_y_attributes
              }), recursive = F),
              simulation_z_networks = unlist(lapply(res_parallel, function(x) {
                x$simulation_z_networks
              }), recursive = F)
            )
          }

          res$stats <- do.call(rbind, lapply(res_parallel, function(x) {
            x$stats
          }))
        }
        # browser()
        if (length(preprocessed$term_names) > 0) {
          if (control$updated_uncertainty) {
            res$var <- var(variability_simulations$gradients_nondegrees)
          } else {
            res$gradients_nondegrees <- variability_simulations$gradients_nondegrees
            res$gradients_degrees <- variability_simulations$gradients_degrees
            V_22 <- var(variability_simulations$gradients_nondegrees)
            V_12 <- var(variability_simulations$gradients_nondegrees, variability_simulations$gradients_degrees)
            if (control$exact == TRUE) {
              V_11 <- var(variability_simulations$gradients_degrees)
              inv_A <- MASS::ginv(res$exact_A)
              tmp <- res$B_mat %*% inv_A
            } else {
              tmp <- sweep(res$B_mat, 2, 1 / res$A_diag, "*")
              V_11 <- diag(apply(X = variability_simulations$gradients_degrees, FUN = var, MARGIN = 2))
            }
            C_2 <- res$fisher_nondegrees - tmp %*% t(res$B_mat)
            C_2_inv <- solve(C_2)
            Y <- -t(tmp) %*% C_2_inv
            Z <- t(Y)
            res$var <- (t(Y) %*% V_11 + C_2_inv %*% V_12) %*% Y + (t(Y) %*% t(V_12) + C_2_inv %*% V_22) %*% C_2_inv
          }
        } else {
          res$var <- matrix(numeric(0), nrow = 0, ncol = 0)
        }

        if (control$return_samples) {
          res$simulations <- lapply(
            seq_along(res$simulations$simulation_x_attributes),
            function(x) {
              XYZ_to_R(
                x_attribute = res$simulations$simulation_x_attributes[[x]],
                y_attribute = res$simulations$simulation_y_attributes[[x]],
                z_network = res$simulations$simulation_z_networks[[x]],
                n_units = n_units, return_adj_mat = F
              )
            }
          )
        }
      }

      if (data_object$directed) {
        rownames(res$coefficients_degrees) <- c(paste(c("out-degrees"), 1:n_units), paste(c("in-degrees"), 1:n_units))
        rownames(res$coefficients_nondegrees) <- preprocessed$coef_names
        if (control$var && length(preprocessed$term_names) > 0) {
          colnames(res$var) <- preprocessed$coef_names
          rownames(res$var) <- preprocessed$coef_names
        }

        colnames(res$coefficients_path) <- c(rownames(res$coefficients_nondegrees), rownames(res$coefficients_degrees))
      } else {
        rownames(res$coefficients_degrees) <- paste(c("degrees"), 1:n_units)
        rownames(res$coefficients_nondegrees) <- preprocessed$coef_names
        if (control$var && length(preprocessed$term_names) > 0) {
          colnames(res$var) <- preprocessed$coef_names
          rownames(res$var) <- preprocessed$coef_names
        }
        colnames(res$coefficients_path) <- c(rownames(res$coefficients_nondegrees), rownames(res$coefficients_degrees))
      }
    } else {
      res <- list()
    }
  } else {
    # Pseudo LH (Nondegrees) ----
    n_units <- length(data_object$x_attribute)

    if (is.null(beg_coef)) {
      coef_tmp <- rep(0, length(preprocessed$term_names))
    } else {
      coef_tmp <- beg_coef
    }

    if (control$estimate_model) {
      res <- pl_estimation(
        coef = coef_tmp,
        data_object$z_network,
        data_object$x_attribute,
        data_object$y_attribute,
        neighborhood = data_object$neighborhood,
        overlap = data_object$overlap,
        directed = data_object$directed,
        terms = preprocessed$term_names,
        max_iteration = control$max_it,
        data_list = preprocessed$data_list,
        nonoverlap_random = nonoverlap_random,
        type_list = preprocessed$type_list,
        display_progress = control$display_progress,
        tol = control$tol,
        offset_nonoverlap = control$offset_nonoverlap,
        non_stop = control$non_stop,
        fix_x = data_object$fix_x,
        fix_z = data_object$fix_z,
        attr_x_family = data_object$family_x,
        attr_y_family = data_object$family_y,
        attr_x_scale = data_object$scale_x,
        attr_y_scale = data_object$scale_y
      )

      ind_droped <- (as.vector(res$where_wrong) + 1)
      if (length(ind_droped) > 0) {
        preprocessed$term_names <- preprocessed$term_names[-ind_droped]
        preprocessed$coef_names <- preprocessed$coef_names[-ind_droped]
        preprocessed$data_list <- preprocessed$data_list[-ind_droped]
        preprocessed$type_list <- preprocessed$type_list[-ind_droped]
      }
      # names(res$coefficients2) = preprocessed$coef_names
      rownames(res$coefficients) <- preprocessed$coef_names
      colnames(res$coefficients_path) <- c(rownames(res$coefficients_nondegrees))


      if (control$var) {
        if (sampler$n_simulation <= 1) {
          warning("Variance estimation requested but sampler has less than 1 simulation. Variance cannot be estimated.")
        }
        if (control$display_progress) {
          cat("Starting with samples to estimate uncertainty \n")
        }

        if (is.null(sampler)) {
          sampler <- sampler.iglm()
          # if no specifications of the sampler are provided use the default one
        }
        if (!is_cluster_active(sampler$cluster)) {
          sampler$set_cluster(NULL)
        }

        if (is.null(sampler$cluster)) {
          variability_simulations <- xyz_approximate_variability(
            coef = res$coefficients, return_samples = control$return_samples,
            coef_degrees = 0,
            terms = preprocessed$term_names,
            n_units = data_object$n_units,
            z_network = data_object$z_network,
            neighborhood = data_object$neighborhood,
            overlap = data_object$overlap,
            x_attribute = data_object$x_attribute,
            y_attribute = data_object$y_attribute,
            family_x = data_object$family_x,
            tnt = sampler$sampler_z$tnt,
            family_y = data_object$family_y,
            nonoverlap_random = nonoverlap_random,
            attr_x_scale = data_object$scale_x,
            attr_y_scale = data_object$scale_y,
            init_empty = sampler$init_empty,
            exact = control$exact,
            directed = data_object$directed,
            data_list = preprocessed$data_list,
            type_list = preprocessed$type_list,
            n_proposals_x = sampler$sampler_x$n_proposals,
            n_proposals_y = sampler$sampler_y$n_proposals,
            seed = sampler$seed,
            n_proposals_z = sampler$sampler_z$n_proposals,
            n_burn_in = sampler$n_burn_in,
            n_simulation = sampler$n_simulation,
            display_progress = control$display_progress,
            degrees = FALSE,
            updated_uncertainty = control$updated_uncertainty,
            offset_nonoverlap = control$offset_nonoverlap,
            fix_x = data_object$fix_x,
            fix_z = data_object$fix_z
          )

          res$simulations <- list(
            simulation_x_attributes = variability_simulations$simulation_x_attributes,
            simulation_y_attributes = variability_simulations$simulation_y_attributes,
            simulation_z_networks = variability_simulations$simulation_z_networks
          )
        } else {
          tmp_split <- split(1:(round(sampler$n_simulation / length(sampler$cluster)) * length(sampler$cluster)), seq_along(sampler$cluster))
          if (control$display_progress) {
            cat("Starting with parallel simulations")
          }

          res_parallel <- parLapply(cl = sampler$cluster, X = tmp_split, fun = function(x, preprocessed,
                                                                                        n_units, res, control) {
            xyz_approximate_variability(
              coef = res$coefficients,
              coef_degrees = 0,
              terms = preprocessed$term_names,
              n_units = n_units,
              z_network = data_object$z_network,
              neighborhood = data_object$neighborhood,
              overlap = data_object$overlap,
              x_attribute = data_object$x_attribute,
              y_attribute = data_object$y_attribute,
              init_empty = sampler$init_empty,
              family_x = data_object$family_x,
              family_y = data_object$family_y,
              nonoverlap_random = nonoverlap_random,
              attr_x_scale = data_object$scale_x,
              attr_y_scale = data_object$scale_y,
              directed = data_object$directed,
              tnt = sampler$sampler_z$tnt,
              data_list = preprocessed$data_list,
              type_list = preprocessed$type_list,
              n_proposals_x = sampler$sampler_x$n_proposals,
              seed = sampler$seed + min(x),
              n_proposals_y = sampler$sampler_y$n_proposals,
              n_proposals_z = sampler$sampler_z$n_proposals,
              n_burn_in = sampler$n_burn_in,
              n_simulation = length(x),
              exact = control$exact,
              updated_uncertainty = control$updated_uncertainty,
              display_progress = control$display_progress,
              degrees = FALSE,
              offset_nonoverlap = control$offset_nonoverlap,
              return_samples = control$return_samples,
              fix_x = data_object$fix_x,
              fix_z = data_object$fix_z
            )
          }, preprocessed = preprocessed, n_units = n_units, res = res, control = control)


          variability_simulations <- list()
          variability_simulations$gradients <- do.call(rbind, lapply(res_parallel, function(x) {
            x$gradients
          }))
          res$simulations <- list(
            simulation_x_attributes = unlist(lapply(res_parallel, function(x) {
              x$simulation_x_attributes
            }), recursive = F),
            simulation_y_attributes = unlist(lapply(res_parallel, function(x) {
              x$simulation_y_attributes
            }), recursive = F),
            simulation_z_networks = unlist(lapply(res_parallel, function(x) {
              x$simulation_z_networks
            }), recursive = F)
          )
          variability_simulations$stats <- do.call(rbind, lapply(res_parallel, function(x) {
            x$stats
          }))
        }
        res$stats <- variability_simulations$stats
        # Corrected variance
        res$var <- res$var %*% var(variability_simulations$gradients) %*% res$var
        if (control$return_samples) {
          res$simulations <- lapply(
            seq_along(res$simulations$simulation_x_attributes),
            function(x) {
              XYZ_to_R(
                x_attribute = res$simulations$simulation_x_attributes[[x]],
                y_attribute = res$simulations$simulation_y_attributes[[x]],
                z_network = res$simulations$simulation_z_networks[[x]],
                n_units = n_units, return_adj_mat = F
              )
            }
          )
        }
        rownames(res$coefficients) <- preprocessed$coef_names
        if (control$var) {
          colnames(res$var) <- preprocessed$coef_names
          rownames(res$var) <- preprocessed$coef_names
        }
        colnames(res$coefficients_path) <- preprocessed$coef_names
      }
    } else {
      res <- list()
    }
  }
  class(res) <- "iglm.info"
  if (return_preprocess) {
    # browser()
    res$preprocess <- xyz_prepare_pseudo_estimation(
      z_network = data_object$z_network,
      x_attribute = data_object$x_attribute,
      y_attribute = data_object$y_attribute,
      neighborhood = data_object$neighborhood,
      overlap = data_object$overlap,
      directed = data_object$directed,
      terms = preprocessed$term_names,
      data_list = preprocessed$data_list,
      type_list = preprocessed$type_list,
      display_progress = control$display_progress,
      return_x = control$return_x,
      return_y = control$return_y,
      return_z = control$return_z,
      family_x = data_object$family_x,
      family_y = data_object$family_y,
      attr_x_scale = data_object$scale_x,
      attr_y_scale = data_object$scale_y
    )

    x <- res$preprocess[[1]]
    y <- names(res$preprocess)[[1]]
    # browser()
    res$preprocess <- mapply(
      x = res$preprocess, y = names(res$preprocess),
      function(x, y) {
        if (y %in% c("res_x", "res_y")) {
          colnames(x$data) <- c("target", "unit", preprocessed$coef_names)
        } else if (y == "res_z") {
          colnames(x$data) <- c("target", "sender", "receiver", "overlapping", preprocessed$coef_names)
        }
        return(x$data)
      }, SIMPLIFY = F
    )
  }
  # browser()
  # res$iglm.object <- iglm.object(formula=formula,
  #                            coef = res$coefficients_nondegrees,
  #                            coef_degrees = res$coefficients_degrees,
  # sampler = sampler)

  res$call <- sys.calls()[[1]]
  return(res)
}
