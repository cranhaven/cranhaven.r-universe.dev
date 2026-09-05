#' Predict Outcomes for New Subjects
#'
#' @description
#' `predict()` method for objects of class `"imr"` produced by [imr()].  New
#' subjects are matched to the availability subgroup models learned during
#' training, missing platforms are handled automatically, and the
#' test features are standardized with the training set's centring and scaling
#' factors before Bayesian model averaging.
#'
#' @param object A fitted object of class `"imr"` returned by [imr()].
#' @param newdata A list of data frames with the new platform measurements.
#'   Each data frame must include `id` as the first column, followed by finite
#'   numeric feature columns matching the corresponding training platform.
#' @param platform_names A character vector giving, for each element of
#'   `newdata`, the index (`"1"`, `"2"`, ...) of the corresponding training
#'   platform.  Defaults to `NULL`, meaning the elements of `newdata` are taken
#'   to be in the same order as the platforms supplied to [imr()].
#' @param covariates An optional data frame of clinical covariates for the test
#'   subjects, including `id` as the first column.  Required when the model was
#'   fitted with covariates and ignored with a warning when it was not.
#' @param max_models Integer; the maximum number of distinct selection models
#'   (gamma configurations) used for Bayesian model averaging.  Default `100`.
#' @param verbose Logical; if `TRUE`, print the C routine's diagnostics.
#'   Defaults to `FALSE`.
#' @param ... Unused; present for S3 compatibility.
#'
#' @details
#' Predictions are only produced for subjects observed on at least one platform
#' (and, when `covariates` is supplied, with covariate data).  For `"binary"`
#' outcomes the returned `predict` column is a probability obtained through the
#' probit link (`pnorm`); for `"continuous"` and `"right.censored"` outcomes it
#' is the predicted (latent) response.
#'
#' @return A named list with one data frame per active availability subgroup
#'   model. Each data frame has columns `id` (subject identifier) and `predict`
#'   (predicted value or probability, rounded to three decimals).
#'
#' @seealso [imr()], [cv_imr()]
#'
#' @examples
#' \donttest{
#' data("simIMR", package = "IntegMultiReg")
#' fit <- imr(
#'   platform_data_list = simIMR$platforms, outcome = simIMR$outcome,
#'   cov = simIMR$covariates, type_outcome = "binary",
#'   nu = c(-4, -3, -4), sample_mcmc = c(200, 100), ssize = 5, seed = 1
#' )
#' new_x <- simIMR$platforms[[1]][1:10, ]
#' new_z <- simIMR$platforms[[2]][1:7, ]
#' predict(fit, newdata = list(new_x, new_z), covariates = simIMR$covariates)
#' }
#' @export
predict.imr <- function(object, newdata, platform_names = NULL,
                        covariates = NULL, max_models = 100,
                        verbose = FALSE, ...) {
  if (!inherits(object, "imr")) {
    .imr_abort("`object` must be an `imr` object returned by `imr()`.")
  }
  .imr_check_flag(verbose, "verbose")
  max_models <- .imr_check_integer_scalar(max_models, "max_models", min = 1)
  type_outcome <- object$type_outcome
  if (is.null(type_outcome)) {
    type_outcome <- "right.censored"
  }
  method <- object$method
  if (is.null(method)) method <- "IMR"
  n_platform <- as.integer(object$data1[[1]])
  n_cov <- as.integer(object$data1[[7]])

  if (!is.list(newdata) || length(newdata) == 0L) {
    .imr_abort("`newdata` must be a non-empty list of data frames.")
  }
  if (length(newdata) > n_platform) {
    .imr_abort("`newdata` cannot contain more platforms than the fitted model.")
  }

  ## Default: the new platforms are supplied in the same order as at training.
  if (is.null(platform_names)) {
    platform_names <- as.character(seq_along(newdata))
  } else {
    if (length(platform_names) != length(newdata)) {
      .imr_abort("`platform_names` must have the same length as `newdata`.")
    }
    platform_names <- as.character(platform_names)
    if (anyNA(platform_names) || anyDuplicated(platform_names)) {
      .imr_abort("`platform_names` must not contain missing or duplicated values.")
    }
    valid_platforms <- as.character(seq_len(n_platform))
    if (any(!platform_names %in% valid_platforms)) {
      .imr_abort(sprintf(
        "`platform_names` must use training platform indices in {%s}.",
        paste(valid_platforms, collapse = ", ")
      ))
    }
  }
  names(newdata) <- platform_names
  model_names <- names((object$data2[[1]])) ### binary indicators of platforms active in each subgroup

  for (i in seq_along(newdata)) {
    arg <- sprintf("newdata[[%d]]", i)
    platform_index <- as.integer(platform_names[i])
    .imr_check_id_frame(newdata[[i]], arg, require_rows = FALSE)
    .imr_check_numeric_columns(newdata[[i]], arg)
    expected_n <- object$data1[[6]][platform_index]
    if (ncol(newdata[[i]]) - 1L != expected_n) {
      .imr_abort(sprintf(
        "`%s` must contain %d feature column(s) for training platform %s.",
        arg, expected_n, platform_names[i]
      ))
    }
    expected_names <- object$feature_names[[platform_index]]
    if (!is.null(expected_names) &&
        !identical(colnames(newdata[[i]])[-1], expected_names)) {
      .imr_abort(sprintf(
        "Feature columns in `%s` must match the training feature names.",
        arg
      ))
    }
  }

  if (n_cov > 0L && is.null(covariates)) {
    .imr_abort("`covariates` is required because the model was fitted with covariates.")
  }
  if (n_cov == 0L && !is.null(covariates)) {
    .imr_warn(
      "`covariates` was supplied, but the model was fitted without covariates; ignoring it."
    )
    covariates <- NULL
  }
  if (!is.null(covariates)) {
    .imr_check_id_frame(covariates, "covariates", require_rows = FALSE)
    .imr_check_numeric_columns(covariates, "covariates")
    if (ncol(covariates) - 1L != n_cov) {
      .imr_abort(sprintf(
        "`covariates` must contain %d covariate column(s).", n_cov
      ))
    }
    expected_covariates <- object$covariate_names
    if (!is.null(expected_covariates) && length(expected_covariates) > 0L &&
        !identical(colnames(covariates)[-1], expected_covariates)) {
      .imr_abort("Columns in `covariates` must match the training covariate names.")
    }
  }

  all_ids <- unique(unlist(lapply(newdata, function(df) df$id)))

  ## We only predict subjects with covariate data that are observed in at least one omics platform
  if (!is.null(covariates)) {
    all_ids <- intersect(all_ids, covariates$id)
  }

  if (length(all_ids) == 0) {
    .imr_warn(
      "No subjects have both the required covariates and at least one platform."
    )
    return(.imr_empty_predictions(model_names))
  }
  # Rows correspond to subjects and columns correspond to platforms.
  presence <- data.frame(do.call(cbind, lapply(newdata, function(df) {
    all_ids %in% df$id
  })))
  names(presence) <- platform_names
  not_active_platform <- setdiff(as.character(1:n_platform), names(presence))
  for (l in not_active_platform) {
    presence[[l]] <- rep(FALSE, length(all_ids))
  }
  presence <- presence[, as.character(c(1:n_platform)), drop = FALSE]

  bitstrings <- apply(
    presence, 1,
    function(x) paste(as.integer(rev(x)), collapse = "")
  )

  x_train <- object$data2[[1]]
  unique_patterns <- model_names
  platforms <- newdata
  for (l in not_active_platform) {
    platforms[[l]] <- data.frame(matrix(nrow = 0, ncol = ncol(x_train[[1]][[as.numeric(l)]])))
  }

  platforms <- platforms[as.character(c(1:n_platform))]
  nplat <- n_platform
  x_test <- list()
  sample_ids <- list()

  for (pat in unique_patterns) {
    subgroup_ids <- all_ids[bitstrings == pat]
    subgroup_list <- vector("list", nplat)
    names(subgroup_list) <- paste0("platform", seq_len(nplat))
    for (i in seq_len(nplat)) {
      bit <- substr(pat, nplat - i + 1, nplat - i + 1)
      if (bit == "1") {
        subgroup_list[[i]] <- .imr_match_rows(
          platforms[[i]], subgroup_ids, sprintf("newdata platform %d", i)
        )
      } else {
        subgroup_list[[i]] <- platforms[[i]][FALSE, , drop = FALSE]
      }
    }
    sample_ids[[pat]] <- subgroup_ids
    x_test[[pat]] <- subgroup_list
  }

  if (length(unlist(sample_ids)) == 0) {
    .imr_warn(
      "No new subjects belong to availability subgroup models retained during training."
    )
    return(.imr_empty_predictions(model_names))
  }
  routed_ids <- unique(unlist(sample_ids, use.names = FALSE))
  dropped_ids <- setdiff(as.character(all_ids), as.character(routed_ids))
  if (length(dropped_ids) > 0L) {
    .imr_warn(sprintf(
      "%d subject(s) do not belong to availability subgroup models retained during training and were not predicted.",
      length(dropped_ids)
    ))
  }

  ### Create a vector that shows the presence of models in the test data
  samplesize_test <- unlist(lapply(sample_ids, length))
  if (!is.null(covariates)) {
    cova_test <- lapply(sample_ids, function(x) {
      .imr_match_rows(covariates, x, "covariates")
    })
  } else {
    cova_test <- lapply(sample_ids, function(x) matrix(numeric(0), nrow = length(x), ncol = 0))
  }
  x_test <- lapply(x_test, function(subgroup) {
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

  if (!is.null(covariates)) {
    cova_test <- lapply(
      seq_along(cova_test),
      function(i) as.matrix(cova_test[[i]][, -1, drop = FALSE])
    )
  }

  norm_mean <- object$data2[[4]]
  norm_sd <- object$data2[[5]]

  x_test <- normalize_nested_list_known_mean_sd(x_test, norm_mean, norm_sd)

  mean_cov <- object$data2[[6]]
  sd_cov <- object$data2[[7]]

  if (!is.null(covariates)) {
    cova_test <- mapply(normalize_matrix_known_mean_variance,
      cova_test, mean_cov, sd_cov,
      SIMPLIFY = FALSE
    )
  }

  method_c <- as.character(method)
  results <- .quietly(verbose, .Call("mainFunctionPredictionTest",
    h0_c = object$list_hyperpara[1],
    hh_c = object$list_hyperpara[2],
    alpha_c = object$list_hyperpara[3],
    psi_c = object$list_hyperpara[4],
    alpha0_c = object$list_hyperpara[5],
    beta0_c = object$list_hyperpara[6],
    seed_c = object$list_hyperpara[7],
    nu_c = object$list_hyperpara[8:(7 + n_platform)],
    y_latent = object$estimate_latent_y,
    gam_sample_c = object$gam_sample,
    theta_c = object$theta_mean,
    method_c = method_c,
    n_platform_c = as.integer(n_platform),
    platform_models_c = object$data1[[2]],
    model_platforms_c = object$data1[[3]],
    n_models = as.integer(object$data1[[4]]),
    sample_size = as.integer(object$data1[[5]]),
    n_features = as.integer(object$data1[[6]]),
    n_cov = as.integer(object$data1[[7]]),
    x_filtered = x_train,
    cov_list = object$data2[[3]],
    ## data1[[9]] is the number of retained MCMC draws (= length(gam_sample));
    ## it indexes the draws used for Bayesian model averaging.
    sample = as.integer(object$data1[[9]]),
    x_test = x_test,
    c_test = cova_test,
    samplesize_test_c = as.integer(samplesize_test),
    max_models_pred = as.integer(max_models)
  ))
  names(results) <- model_names
  if (type_outcome == "binary") {
    res <- mapply(function(x, y) {
      data.frame(
        id = x,
        predict = round(pnorm(y), digits = 3),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }, sample_ids, results, SIMPLIFY = FALSE)
  } else {
    res <- mapply(function(x, y) {
      data.frame(
        id = x,
        predict = round(y, digits = 3),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }, sample_ids, results, SIMPLIFY = FALSE)
  }

  names(res) <- paste("model:", model_names, sep = "")
  return(res)
}
