#' Computing Linear Contrasts
#' @description A set of helper functions for computing linear contrasts of AW-AIPW, IPW, and OLS estimates
#' @name lin_contrast
#' @family estimation
#' @keywords internal
NULL

#' Build Contrast Matrices for Pairwise Comparisons
#' @describeIn lin_contrast Constructs a matrix of contrast vectors for testing pairwise comparisons
#' between treatment arms.
#' @inheritParams run_mab_single
#' @inheritParams mab_from_rct
#' @param bandits data.frame or data.table of bandit statistics
#'
#' @returns A list of 1 row contrast vectors, encoding a single pairwise comparison.
#' @keywords internal
build_contrast_matrices <- function(
  conditions,
  contrasts = NULL,
  bandits
) {
  control_contrasts <- NULL
  best_contrasts <- NULL
  all_contrasts <- NULL

  if (isTRUE(contrasts %in% c("control", "both"))) {
    control_idx <- which(names(conditions) == "control")
    control_contrasts <- make_contrasts(
      conditions,
      control_idx,
      type = "control"
    )
  }
  if (isTRUE(contrasts %in% c("best", "both"))) {
    best_idx <- if (data.table::is.data.table(bandits)) {
      which.max(bandits[
        nrow(bandits),
        .SD,
        .SDcols = conditions
      ])
    } else {
      which.max(bandits[
        nrow(bandits),
        conditions
      ])
    }
    best_contrasts <- make_contrasts(conditions, best_idx, type = "best")
  }

  if (isTRUE(contrasts == "all")) {
    all_contrasts <- clubSandwich::constrain_pairwise(
      1:length(conditions),
      coefs = conditions
    )
  }

  combined <- unique(c(control_contrasts, best_contrasts, all_contrasts))
  names(combined) <- vapply(
    combined,
    \(x) {
      paste0(conditions[x == 1], " - ", conditions[x == -1])
    },
    character(1)
  )
  return(combined)
}

#' Make Contrast Vectors
#' @describeIn lin_contrast Creates contrast vectors for linear hypothesis test
#' @param conditions Vector of treatment conditions
#' @param ref_idx Index of the reference arm
#' @param type Type of contrasts, "best" or "control"
#' @returns A matrix row vectors containing each contrast to test.
#' @keywords internal
make_contrasts <- function(conditions, ref_idx, type) {
  k <- length(conditions)
  others <- setdiff(seq_len(k), ref_idx)

  lapply(
    others,
    \(i) {
      C <- matrix(0, nrow = 1, ncol = k)
      if (type == "best") {
        C[1, ref_idx] <- 1
        C[1, i] <- -1
      } else {
        C[1, ref_idx] <- -1
        C[1, i] <- 1
      }
      return(C)
    }
  )
}


#' Compute Linear Contrasts from Simulation Estimators
#'
#' @describeIn lin_contrast Compute linear contrasts given a list of contrast vectors
#'   and model inputs. Dispatches to the appropriate method based on the
#'   estimator type and whether clustering is present.
#'
#' @param C List of 1 x k contrast vectors
#' @param coefs named vector of coefficients
#' @param vcov A `k x k` variance-covariance matrix
#' @param df Numeric scalar giving the degrees of freedom for the t-test. AW-AIPW only uses t-test in
#' clustered case.
#' @param model An `lm_robust` model object
#' @param dt Whether to compile results into a data.table or data.frame
#' @inheritParams compute_iaipw
#' @inheritParams run_mab
#' @inheritParams estimate_aw_aipw
#' @inheritParams estimate_lm_bundle
#' @returns data.table or data.frame of linear contrasts with columns for each treatment arm, the
#' estimated contrast, standard error, and degrees of freedom.
#'
compute_contrast <- function(
  C = NULL,
  coefs = NULL,
  vcov = NULL,
  df = NULL,
  model = NULL,
  data = NULL,
  dt,
  conditions = NULL,
  estimator
) {
  if (is.null(C)) {
    return(NULL)
  }
  as_df_func <- if (dt) data.table::as.data.table else tibble::as_tibble
  df_func <- if (dt) data.table::data.table else tibble::tibble
  if (!is.null(model)) {
    vcr <- clubSandwich::vcovCR(model, inverse_var = NULL)
    dimnames(vcr) <- lapply(dimnames(vcr), \(x) {
      gsub(
        "^mab_condition",
        "",
        x
      )
    })
    res <- clubSandwich::linear_contrast(
      model,
      vcov = vcr,
      contrasts = C,
      test = "Satterthwaite",
      p_values = FALSE
    ) |>
      tidyr::separate(Coef, into = c("arm1", "arm2"), sep = " - ") |>
      dplyr::select(arm1, arm2, Est, SE, df) |>
      as_df_func()
    colnames(res) <- tolower(colnames(res))
    res[["estimator"]] <- estimator
  } else {
    res <- df_func(
      arm1 = vapply(
        C,
        \(x) {
          conditions[x == 1]
        },
        FUN.VALUE = character(1)
      ),
      arm2 = vapply(
        C,
        \(x) {
          conditions[x == -1]
        },
        FUN.VALUE = character(1)
      ),
      est = vapply(
        C,
        \(x) {
          as.numeric(x %*% coefs)
        },
        FUN.VALUE = numeric(1)
      ),
      se = vapply(
        C,
        \(x) {
          sqrt(x %*% vcov %*% t(x)) |> as.numeric()
        },
        numeric(1)
      ),
      df = df,
      estimator = estimator
    )
  }
  return(res)
}

#' Fit a Robust Linear Probability Model and Compute Contrasts
#' @name estimate_lm_bundle
#'
#' @description Internal helper that wraps linear model estimation for both IPW
#' and OLS estimators, returning arm-level mean estimates, precomputed
#' contrasts, and optionally the fitted model object.
#'
#' @param estimator Character string labeling the estimator, either
#'   `"IPW"` or `"OLS"`
#' @inheritParams run_mab
#' @inheritParams estimate_aw_aipw
#' @inheritParams estimate_lm
#' @inheritParams run_mab_single
#' @param contrasts_list List of 1 x k row vector contrasts to compute.
#'
#' @returns A named list with three elements: `means`, `contrasts`, `model`. `contrasts` are the
#' computed contrasts, the other 2 elements are the output of [estimate_lm()]
#'
#' @keywords internal
estimate_lm_bundle <- function(
  ipw,
  estimator,
  sim_results,
  col_names,
  clustering,
  conditions,
  num_clusters,
  dt,
  contrasts_list
) {
  means <- estimate_lm(
    data = sim_results[["final_data"]],
    cluster_col = col_names[["cluster_col"]],
    clustering = clustering,
    conditions = conditions,
    num_clusters = num_clusters,
    ipw = ipw
  )
  contrasts <- if (clustering) {
    compute_contrast(
      C = contrasts_list,
      model = means[["model"]],
      estimator = estimator,
      dt = dt,
      data = sim_results[["final_data"]]
    )
  } else {
    compute_contrast(
      C = contrasts_list,
      coefs = means[["estimates"]][["mean"]],
      vcov = diag(means[["estimates"]][["se"]]^2),
      df = unique(means[["estimates"]][["df"]]),
      estimator = estimator,
      dt = dt,
      conditions = conditions
    )
  }
  list(
    means = fill_missing_conditions(
      means[["estimates"]],
      conditions,
      estimator
    ),
    f_stat = means[["f_stat"]],
    contrasts = contrasts,
    model = means[["model"]]
  )
}
