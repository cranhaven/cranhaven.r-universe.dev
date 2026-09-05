#' Fit the Integrative Bayesian Multi-Platform Regression Model
#'
#' @description
#' `imr()` fits the integrative multi-regression (IMR) model of Chekouo et al.
#' (2017) by Markov chain Monte Carlo (MCMC).
#' It identifies biomarkers associated with a time-to-event, binary or
#' continuous outcome while borrowing information across all subjects,
#' regardless of which platforms each subject has measured.  Subjects are
#' partitioned into availability subgroups defined by their pattern of platform
#' availability; one regression is built per subgroup and information is shared
#' across subgroups through a Markov random field (MRF) prior on the
#' variable-selection indicators together with non-local priors on the
#' regression coefficients.  The sampler is implemented in C for efficiency.
#'
#' The arguments are grouped by the orthogonal aspect of the analysis that each
#' one controls: the *data* (`platform_data_list`, `outcome`, `cov`), the
#' *likelihood* (`type_outcome`), the *model* (`method`), subgroup *filtering*
#' (`ssize`), the *priors* (`nu`, `hh`, `h0`, `sig_alpha_psi`, `thet_alph_bet`),
#' the *computation* (`sample_mcmc`, `seed`) and the *output* (`verbose`).
#'
#' @param platform_data_list A list of data frames, one per platform.  Each
#'   data frame must contain an `id` column (the subject identifier, taken to be
#'   the first column); the remaining columns are finite numeric features
#'   measured on that platform.  Subject identifiers must be unique within each
#'   data frame.  The `id` column links subjects across platforms and to the
#'   outcome and covariate data.
#' @param outcome A data frame containing an `id` column and the response.  For
#'   `type_outcome = "right.censored"` it must also contain the event/censoring
#'   time and a censoring indicator (three columns in total).  For `"binary"`
#'   the response must be coded 0/1 and for `"continuous"` it is a numeric
#'   response (two columns in total).  The `id` column must be first and unique.
#' @param cov An optional data frame of clinical covariates including an `id`
#'   column followed by finite numeric covariates.  Covariates are always
#'   included in every regression (they are not subject to selection).  Defaults
#'   to `NULL` (no covariates).
#' @param type_outcome Character string specifying the outcome type, one of
#'   `"right.censored"` (default), `"binary"` or `"continuous"`.
#' @param method Character string specifying the method, `"IMR"` (default) for
#'   the integrative model that shares information across subgroups via the MRF
#'   prior, or `"BMS"` for the non-integrative Bayesian multi-step variant that
#'   fits each subgroup independently (MRF interaction parameters set to zero).
#' @param ssize Minimum availability subgroup size for a subgroup to be
#'   modelled. Subgroups with at most `ssize` subjects are dropped. Default is
#'   `30`.
#' @param nu A numeric vector of prior log-odds of inclusion, one value per
#'   platform, controlling the prior sparsity of the selected features.
#'   Defaults to `rep(-3, length(platform_data_list))`.
#' @param hh Scale of the non-local (product moment) prior on the slab
#'   regression effects (default `0.087`).
#' @param h0 Prior variance of the intercept (default `10000`).
#' @param sig_alpha_psi Length-2 numeric vector with the shape and rate of the
#'   inverse-gamma prior on the response error variance (default
#'   `c(0.001, 0.001)`).  Ignored when `type_outcome = "binary"`: a probit model
#'   has residual variance fixed at 1 for identifiability, so the error variance
#'   is pinned to 1 rather than estimated.
#' @param thet_alph_bet Length-2 numeric vector with the shape and rate of the
#'   gamma prior on the MRF interaction parameters theta, which borrow
#'   information across subgroups (default `c(40, 10)`).
#' @param sample_mcmc An integer vector `c(n_retained, n_burnin)` giving the
#'   number of post-burn-in draws to retain and the number of burn-in
#'   iterations to discard.  The sampler runs `n_retained + n_burnin`
#'   iterations in total, so the returned `log_posterior` has
#'   `sum(sample_mcmc)` entries.  Default is `c(2000, 1000)`.
#' @param seed Optional integer seed for the sampler.  If `NULL` (the default) a
#'   seed is drawn from the current R RNG state, so a run is reproducible
#'   whenever [set.seed()] is called beforehand or an explicit `seed` is passed.
#' @param verbose Logical; if `TRUE`, print the sampler's progress and
#'   diagnostics to the console.  Defaults to `FALSE` (quiet).
#'
#' @details
#' All feature and covariate data are standardized internally (mean 0, standard
#' deviation 1); the centring and scaling factors are stored in the returned
#' object so that [predict.imr()] can apply the same transformation to new
#' subjects.  For right-censored outcomes the latent log-survival times of
#' censored subjects are imputed within the sampler; for binary outcomes a
#' probit data-augmentation latent variable is sampled.
#'
#' @return An object of class `"imr"`: a list with components
#' \item{gam_mean}{List (one matrix per platform) of posterior mean
#'   variable-selection probabilities; rows index the subgroups containing the
#'   platform and columns index the platform features.}
#' \item{theta_mean}{List (one matrix per platform) of posterior mean MRF
#'   interaction parameters between subgroups.}
#' \item{estimate_latent_y}{List (one vector per subgroup) of posterior mean
#'   latent responses, useful for censored or binary data.}
#' \item{log_posterior}{Numeric vector of log-posterior values across all MCMC
#'   iterations (burn-in included).}
#' \item{gam_sample}{Post-burn-in MCMC samples of the selection indicators.}
#' \item{theta_sample}{Post-burn-in MCMC samples of the MRF interaction
#'   parameters (absent when `method = "BMS"`).}
#' \item{list_hyperpara, data1, data2}{Hyper-parameters and pre-processed data
#'   retained for prediction and cross-validation.}
#' \item{call, type_outcome, n_platform, platform_names, feature_names,
#'   covariate_names, model_bitstrings, sample_size, model_platforms,
#'   platform_models, sample_mcmc, nu, method}{Run metadata used by the print,
#'   summary, plot and predict methods.}
#'
#' @references
#' Chekouo T, Stingo FC, Doecke JD, Do K-A (2017). "A Bayesian Integrative
#' Approach for Multi-Platform Genomic Data: A Kidney Cancer Case Study."
#' \emph{Biometrics}, \strong{73}(2), 615--624. \doi{10.1111/biom.12587}
#'
#' @seealso [predict.imr()], [cv_imr()], [summary.imr()], [plot.imr()]
#'
#' @examples
#' \donttest{
#' data("simIMR", package = "IntegMultiReg")
#' fit <- imr(
#'   platform_data_list = simIMR$platforms,
#'   outcome = simIMR$outcome,
#'   cov = simIMR$covariates,
#'   type_outcome = "binary",
#'   nu = c(-4, -3, -4),
#'   sample_mcmc = c(200, 100),
#'   ssize = 5,
#'   seed = 1
#' )
#' fit
#' }
#' @export
imr <- function(platform_data_list,
                                           outcome,
                                           cov = NULL,
                                           type_outcome = c("right.censored", "binary", "continuous"),
                                           method = c("IMR", "BMS"),
                                           ssize = 30,
                                           nu = rep(-3, length(platform_data_list)),
                                           hh = 0.087,
                                           h0 = 10000,
                                           sig_alpha_psi = c(0.001, 0.001),
                                           thet_alph_bet = c(40, 10),
                                           sample_mcmc = c(2000, 1000),
                                           seed = NULL,
                                           verbose = FALSE) {
  cl <- match.call()
  type_outcome <- match.arg(type_outcome)
  method <- match.arg(method)

  .imr_check_flag(verbose, "verbose")
  if (!is.list(platform_data_list) || length(platform_data_list) == 0L) {
    .imr_abort("`platform_data_list` must be a non-empty list of data frames.")
  }
  n_platform <- length(platform_data_list)
  for (i in seq_along(platform_data_list)) {
    arg <- sprintf("platform_data_list[[%d]]", i)
    .imr_check_id_frame(platform_data_list[[i]], arg)
    .imr_check_numeric_columns(platform_data_list[[i]], arg)
  }

  .imr_check_id_frame(outcome, "outcome")
  if (type_outcome %in% c("binary", "continuous")) {
    if (ncol(outcome) != 2L) {
      .imr_abort(
        "`outcome` must have exactly two columns: `id` and the response."
      )
    }
    .imr_check_numeric_columns(outcome, "outcome", names(outcome)[2])
    if (type_outcome == "binary" && !all(outcome[[2]] %in% c(0, 1))) {
      .imr_abort("For `type_outcome = \"binary\"`, the response must be coded 0/1.")
    }
  } else if (type_outcome == "right.censored") {
    if (ncol(outcome) != 3L) {
      .imr_abort(
        paste0("`outcome` must have exactly three columns for right-censored ",
               "data: `id`, time and status.")
      )
    }
    .imr_check_numeric_columns(outcome, "outcome", names(outcome)[2:3])
    if (any(outcome[[2]] <= 0)) {
      .imr_abort("Right-censored event times in `outcome` must be positive.")
    }
    if (!all(outcome[[3]] %in% c(0, 1))) {
      .imr_abort("Right-censored status values in `outcome` must be coded 0/1.")
    }
  }

  if (!is.null(cov)) {
    .imr_check_id_frame(cov, "cov")
    .imr_check_numeric_columns(cov, "cov")
  }

  ssize <- .imr_check_integer_scalar(ssize, "ssize", min = 0)
  nu <- .imr_check_numeric_vector(nu, "nu", length = n_platform)
  h0 <- .imr_check_numeric_vector(h0, "h0", length = 1, positive = TRUE)
  hh <- .imr_check_numeric_vector(hh, "hh", length = 1, positive = TRUE)
  sig_alpha_psi <- .imr_check_numeric_vector(
    sig_alpha_psi, "sig_alpha_psi", length = 2, positive = TRUE
  )
  thet_alph_bet <- .imr_check_numeric_vector(
    thet_alph_bet, "thet_alph_bet", length = 2, positive = TRUE
  )
  sample_mcmc <- .imr_check_integer_vector(
    sample_mcmc, "sample_mcmc", length = 2, min = 0
  )
  if (sample_mcmc[1] <= 0L) {
    .imr_abort("`sample_mcmc[1]` (retained draws) must be positive.")
  }
  if (!is.null(seed)) {
    seed <- .imr_check_integer_scalar(seed, "seed", min = 0)
  }

  ## The sampler draws on both R's RNG and a GSL RNG seeded by `seed`.  With an
  ## explicit seed we call set.seed() so the run is fully reproducible; when
  ## `seed` is NULL we instead draw one from the ambient R RNG, so results follow
  ## the user's own set.seed() like other R modelling functions.
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  } else {
    set.seed(seed)
  }
  type_out <- 1
  if (type_outcome == "binary") {
    type_out <- 2
  } else if (type_outcome == "continuous") {
    type_out <- 3
  }

  ## Record human-readable platform and feature names (the first column of each
  ## platform is the 'id' and is dropped before modelling).
  platform_names <- names(platform_data_list)
  if (is.null(platform_names) || any(platform_names == "")) {
    platform_names <- paste0("platform", seq_len(n_platform))
  }
  feature_names <- lapply(platform_data_list, function(df) colnames(df)[-1])

  dat <- subgroup_data(outcome, cov, platform_data_list)
  n_sample <- sample_mcmc[1]
  n_burnin <- sample_mcmc[2]
  # Prepare scalar parameters.
  h0_c <- as.numeric(h0)
  hh_c <- as.numeric(hh)
  alpha_c <- as.numeric(sig_alpha_psi[1])
  psi_c <- as.numeric(sig_alpha_psi[2])
  if (type_outcome == "binary") {
    # A probit outcome fixes the residual variance at 1 for identifiability
    # (the latent utility is z = eta + e with e ~ N(0, 1)).  The marginal
    # likelihood otherwise integrates sigma^2 out under this inverse-gamma
    # prior, which leaves the latent scale only weakly identified and makes the
    # binary chain drift and mix poorly.  Concentrating the prior at 1 pins
    # sigma^2 = 1; any large shape/rate gives an effectively fixed unit variance.
    probit_unit_variance <- 1e5
    alpha_c <- psi_c <- probit_unit_variance
  }
  alpha0_c <- as.numeric(thet_alph_bet[1])
  beta0_c <- as.numeric(thet_alph_bet[2])
  seed_c <- as.numeric(seed)

  storage.mode(h0_c) <- "double"
  storage.mode(hh_c) <- "double"
  storage.mode(alpha_c) <- "double"
  storage.mode(psi_c) <- "double"
  storage.mode(alpha0_c) <- "double"
  storage.mode(beta0_c) <- "double"
  storage.mode(seed_c) <- "double"

  method_c <- as.character(method)

  #################################################################
  # Process the platform data and extract subgroup id vectors.
  #################################################################
  dat_orig <- dat # Preserve the original data (with "id" columns intact)
  # For each subgroup, extract the id vector from the first nonempty platform.
  dat_ids <- lapply(dat_orig[[3]], function(subgroup) {
    nonempty <- which(sapply(subgroup, function(platform_data) {
      if (is.data.frame(platform_data) && "id" %in% colnames(platform_data)) {
        return(nrow(platform_data))
      } else {
        return(0)
      }
    }) > 0)
    if (length(nonempty) > 0) {
      return(subgroup[[nonempty[1]]]$id)
    } else {
      return(character(0))
    }
  })

  # Now convert the platform data into a list of numeric matrices by dropping
  # the "id" column.
  dat[[3]] <- lapply(dat[[3]], function(subgroup) {
    lapply(subgroup, function(platform_data) {
      if (is.data.frame(platform_data) && "id" %in% colnames(platform_data)) {
        mat <- as.matrix(platform_data[, -1, drop = FALSE])
      } else if (!is.null(colnames(platform_data)) &&
        colnames(platform_data)[1] == "id") {
        mat <- as.matrix(platform_data[, -1, drop = FALSE])
      } else {
        mat <- as.matrix(platform_data)
      }
      storage.mode(mat) <- "double"
      return(mat)
    })
  })

  n_features <- as.integer(vapply(
    platform_data_list, function(x) ncol(x) - 1L, integer(1)
  ))
  # Compute representative row counts for each subgroup.
  subgroup_rows <- sapply(dat[[3]], function(subgroup) {
    nonempty <- which(sapply(subgroup, function(mat) nrow(mat)) > 0)
    if (length(nonempty) > 0) {
      nrow(subgroup[[nonempty[1]]])
    } else {
      0
    }
  })

  # Keep only subgroups with more than 'ssize' rows.
  model_index <- as.integer(which(subgroup_rows > ssize))
  if (length(model_index) == 0) {
    .imr_abort(
      "No availability subgroup has more than `ssize` subjects; lower `ssize` or check the data."
    )
  }
  sample_size <- as.integer(subgroup_rows[model_index])

  # Filter the platform data to include only subgroups meeting the threshold.
  dat_filtered <- lapply(dat, function(x) x[model_index])

  # Also filter the original id vectors accordingly.
  subgroup_ids_filtered <- dat_ids[model_index]

  #############################################################
  # Process the response and covariates using the filtered subgroup ids.
  #############################################################
  ### Match ids and drop the id column from the outcome and covariate frames.
  dat_filtered[[1]] <- lapply(
    seq_along(dat_filtered[[1]]),
    function(i) {
      .imr_match_rows(
        dat_filtered[[1]][[i]], subgroup_ids_filtered[[i]], "outcome"
      )[, -1, drop = FALSE]
    }
  )
  if (!is.null(cov)) {
    dat_filtered[[2]] <- lapply(
      seq_along(dat_filtered[[2]]),
      function(i) {
        .imr_match_rows(
          dat_filtered[[2]][[i]], subgroup_ids_filtered[[i]], "cov"
        )[, -1, drop = FALSE]
      }
    )
  }

  dat_normalized <- dat_filtered

  mean_train <- mean_nested_list(dat_filtered[[3]])
  sd_train <- sd_nested_list(dat_filtered[[3]])
  if (!is.null(cov)) {
    mean_cov_train <- lapply(dat_filtered[[2]], mean_matrix)
    sd_cov_train <- lapply(dat_filtered[[2]], sd_matrix)
  } else {
    mean_cov_train <- NULL
    sd_cov_train <- NULL
  }

  dat_normalized[[3]] <- normalize_nested_list(dat_filtered[[3]])
  ### Normalized covariates
  if (!is.null(cov)) {
    dat_normalized[[2]] <- lapply(dat_filtered[[2]], normalize_matrix)
  }
  ## outcome: force double storage (the C sampler reads it with REAL())
  dat_normalized[[1]] <- lapply(dat_filtered[[1]], function(x) {
    m <- as.matrix(x)
    storage.mode(m) <- "double"
    m
  })

  if (verbose) {
    cat("Sample sizes for modelled availability subgroups:", sample_size, "\n")
  }

  ### For each model, list the corresponding platforms involved in the model
  n_models <- length(model_index)

  model_platforms_c <- sapply(1:n_models, function(x) {
    as.integer((seq_along(dat_normalized[[3]][[x]]) - 1)[unlist(lapply(
      seq_along(dat_normalized[[3]][[x]]),
      function(i) nrow(dat_normalized[[3]][[x]][[i]])
    )) > 0])
  }, simplify = FALSE)

  ### For each platform, obtain the model indices where that platform is involved
  platform_models_c <- sapply(1:n_platform, function(x) {
    as.integer((seq(1, n_models) - 1)[unlist(lapply(
      model_platforms_c,
      function(y) (x - 1) %in% y
    ))])
  }, simplify = FALSE)

  n_platform_c <- n_platform
  x_filtered <- dat_normalized[[3]]
  y_list <- dat_normalized[[1]]

  if (!is.null(cov)) {
    n_cov <- ncol(dat_normalized[[2]][[1]])
  } else {
    n_cov <- 0
  }
  if (n_cov == 0) {
    cov_list <- lapply(y_list, function(x) {
      matrix(numeric(0), nrow = nrow(x), ncol = 0)
    })
  } else {
    cov_list <- dat_normalized[[2]]
  }

  nu_c <- as.double(nu)

  ###########################################
  # Call the compiled MCMC sampler.
  ###########################################
  results <- .quietly(verbose, .Call("mainFunction", h0_c, hh_c, alpha_c, psi_c,
    alpha0_c, beta0_c, seed_c, nu_c, method_c,
    n_platform_c = as.integer(n_platform_c),
    platform_models_c = platform_models_c, model_platforms_c = model_platforms_c,
    n_models = as.integer(n_models),
    sample_size = as.integer(sample_size),
    n_features = as.integer(n_features),
    n_cov = as.integer(n_cov),
    x_filtered = x_filtered, y_list = y_list,
    type_outcome = as.integer(type_out),
    cov_list = cov_list,
    sample = as.integer(n_sample),
    burnin = as.integer(n_burnin)
  ))

  ## Guard against tiny floating-point drift in the running averages so that
  ## the reported inclusion probabilities are exactly within [0, 1].
  results$gam_mean <- lapply(results$gam_mean, function(m) {
    m[m > 1] <- 1
    m[m < 0] <- 0
    m
  })

  results$list_hyperpara <- c(
    h0_c, hh_c, alpha_c, psi_c, alpha0_c, beta0_c, seed_c, nu_c
  )
  # total number of hyper-parameters = 7 + n_platform

  results$data1 <- list(
    n_platform_c, platform_models_c, model_platforms_c,
    n_models, sample_size, n_features, n_cov, type_out,
    n_sample
  )
  results$data2 <- list(
    xx = x_filtered, yy = y_list, cc = cov_list,
    mean_train, sd_train, mean_cov_train, sd_cov_train
  )

  ## Run metadata for the print / summary / plot / predict methods.
  model_bitstrings <- names(x_filtered)
  results$call <- cl
  results$type_outcome <- type_outcome
  results$method <- method
  results$n_platform <- n_platform
  results$platform_names <- platform_names
  results$feature_names <- feature_names
  results$covariate_names <- if (!is.null(cov)) colnames(cov)[-1] else character(0)
  results$model_bitstrings <- model_bitstrings
  results$sample_size <- stats::setNames(sample_size, model_bitstrings)
  results$model_platforms <- lapply(model_platforms_c, function(x) x + 1L)
  results$platform_models <- lapply(platform_models_c, function(x) x + 1L)
  results$nu <- nu
  results$ssize <- ssize
  results$sample_mcmc <- c(total = n_sample, burnin = n_burnin)

  class(results) <- "imr"
  return(results)
}


#' @keywords internal
#' @noRd
subgroup_data <- function(outcome, cov = NULL, platform_data_list) {
  # Collect all the input data frames into a list
  nplat <- length(platform_data_list)
  ### Intersection of outcome and covariate ids with the union of platform ids
  ids_out_cov <- outcome$id
  if (!is.null(cov)) {
    ids_out_cov <- intersect(outcome$id, cov$id)
  }

  platforms <- lapply(platform_data_list, function(x) {
    x[x$id %in% ids_out_cov, , drop = FALSE]
  })

  id_outcome <- unique(unlist(lapply(platforms, function(x) x$id)))
  if (length(id_outcome) == 0L) {
    .imr_abort(
      "No subjects have both outcome/covariate data and at least one platform."
    )
  }
  outcome1 <- outcome[outcome$id %in% id_outcome, , drop = FALSE]
  cov1 <- if (!is.null(cov)) {
    cov[cov$id %in% id_outcome, , drop = FALSE]
  } else {
    NULL
  }

  # Check that each platform has at least one column named "id"
  if (!all(sapply(platforms, function(df) "id" %in% colnames(df)))) {
    .imr_abort("Each input data frame must have an `id` column.")
  }

  # Create the union of all ids across platforms
  all_ids <- unique(unlist(lapply(platforms, function(df) df$id)))

  # Build a presence matrix (rows = subjects, columns = platforms).
  presence <- do.call(cbind, lapply(platforms, function(df) all_ids %in% df$id))

  # Create a binary string for each subject.
  # Convention: the rightmost digit is presence in the first platform, etc.
  bitstrings <- apply(presence, 1, function(x) paste(as.integer(rev(x)), collapse = ""))

  # Identify the unique binary patterns (subgroups) sorted in ascending order.
  unique_patterns <- sort(unique(bitstrings))

  x1 <- list()
  sample_ids <- list()

  for (pat in unique_patterns) {
    subgroup_ids <- all_ids[bitstrings == pat]
    subgroup_list <- vector("list", nplat)
    names(subgroup_list) <- paste0("platform", seq_len(nplat))
    for (i in seq_len(nplat)) {
      # For the i-th platform, the corresponding bit is at position nplat - i + 1.
      bit <- substr(pat, nplat - i + 1, nplat - i + 1)
      if (bit == "1") {
        subgroup_list[[i]] <- .imr_match_rows(
          platforms[[i]], subgroup_ids, sprintf("platform_data_list[[%d]]", i)
        )
      } else {
        subgroup_list[[i]] <- platforms[[i]][FALSE, , drop = FALSE]
      }
    }
    sample_ids[[pat]] <- subgroup_ids
    x1[[pat]] <- subgroup_list
  }
  ## Align the outcome and covariate rows to each subgroup's canonical subject
  ## order (`sample_ids`, taken from the platform data) with match(), rather than
  ## relying on the inputs sharing the same row order.  The compiled sampler
  ## pairs outcome / covariate / platform rows by position, so this keeps them
  ## correctly aligned even when the outcome or covariate frames are supplied in
  ## a different order (for id-sorted inputs it is a no-op).
  outcome2 <- lapply(sample_ids, function(x) .imr_match_rows(outcome1, x, "outcome"))
  cov2 <- if (!is.null(cov1)) {
    lapply(sample_ids, function(x) .imr_match_rows(cov1, x, "cov"))
  } else {
    lapply(sample_ids, function(x) data.frame(id = x))
  }

  return(list(outcome = outcome2, covariate = cov2, platform_data = x1))
}

#' @keywords internal
#' @noRd
normalize_matrix <- function(mat) {
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(matrix(numeric(0), nrow = nrow(mat), ncol = ncol(mat)))
  }
  nm <- apply(mat, 2, function(col) {
    m <- mean(col, na.rm = TRUE)
    s <- sd(col, na.rm = TRUE)
    if (is.na(s) || s == 0) {
      rep(0, length(col))
    } else {
      (col - m) / s
    }
  })
  if (is.null(dim(nm))) {
    nm <- matrix(nm, nrow = nrow(mat), ncol = ncol(mat))
  }
  return(nm)
}

#' @keywords internal
#' @noRd
mean_matrix <- function(mat) {
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(rep(0, ncol(mat)))
  }
  apply(mat, 2, function(col) mean(col, na.rm = TRUE))
}

#' @keywords internal
#' @noRd
sd_matrix <- function(mat) {
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(rep(1, ncol(mat)))
  }
  apply(mat, 2, function(col) {
    s <- sd(col, na.rm = TRUE)
    if (is.na(s) || s == 0) 1 else s
  })
}

#' @keywords internal
#' @noRd
normalize_matrix_known_mean_variance <- function(mat, mean, sd) {
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(matrix(numeric(0), nrow = nrow(mat), ncol = ncol(mat)))
  }
  nm <- sapply(1:NCOL(mat), function(col) {
    (mat[, col] - mean[col]) / sd[col]
  })
  if (is.null(dim(nm))) {
    nm <- matrix(nm, nrow = nrow(mat), ncol = ncol(mat))
  }
  return(nm)
}

#' @keywords internal
#' @noRd
normalize_nested_list <- function(nested_list) {
  lapply(nested_list, function(subgroup) lapply(subgroup, normalize_matrix))
}

#' @keywords internal
#' @noRd
mean_nested_list <- function(nested_list) {
  lapply(nested_list, function(subgroup) lapply(subgroup, mean_matrix))
}

#' @keywords internal
#' @noRd
sd_nested_list <- function(nested_list) {
  lapply(nested_list, function(subgroup) lapply(subgroup, sd_matrix))
}

#' @keywords internal
#' @noRd
normalize_nested_list_known_mean_sd <- function(nested_list, mean_nested_list, sd_nested_list) {
  mapply(function(x, y, z) {
    mapply(normalize_matrix_known_mean_variance, x, y, z, SIMPLIFY = FALSE)
  }, nested_list, mean_nested_list, sd_nested_list, SIMPLIFY = FALSE)
}
